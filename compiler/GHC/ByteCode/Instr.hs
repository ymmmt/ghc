
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode instruction definitions
module GHC.ByteCode.Instr (
        BCInstr(..), ProtoBCO(..), bciStackUse, LocalLabel(..)
  ) where

import GHC.Prelude

import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHCi.FFI (C_ffi_cif)
import GHC.StgToCmm.Layout     ( ArgRep(..) )
import GHC.Utils.Outputable
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Literal
import GHC.Core.DataCon
import GHC.Builtin.PrimOps
import GHC.Runtime.Heap.Layout

import Data.Word
import GHC.Stack.CCS (CostCentre)

import GHC.Stg.Syntax

-- ----------------------------------------------------------------------------
-- Bytecode instructions

data ProtoBCO a
   = ProtoBCO {
        protoBCOName       :: a,          -- name, in some sense
        protoBCOInstrs     :: [BCInstr],  -- instrs
        -- arity and GC info
        protoBCOBitmap     :: [StgWord],
        protoBCOBitmapSize :: Word16,
        protoBCOArity      :: Int,
        -- what the BCO came from, for debugging only
        protoBCOExpr       :: Either [CgStgAlt] CgStgRhs,
        -- malloc'd pointers
        protoBCOFFIs       :: [FFIInfo]
   }

-- | A local block label (e.g. identifying a case alternative).
newtype LocalLabel = LocalLabel { getLocalLabel :: Word32 }
  deriving (Eq, Ord)

instance Outputable LocalLabel where
  ppr (LocalLabel lbl) = text "lbl:" <> ppr lbl

data BCInstr
   -- Messing with the stack
   = STKCHECK  Word

   -- Push locals (existing bits of the stack)
   | PUSH_L    !Word16{-offset-}
   | PUSH_LL   !Word16 !Word16{-2 offsets-}
   | PUSH_LLL  !Word16 !Word16 !Word16{-3 offsets-}

   -- Push the specified local as a 8, 16, 32 bit value onto the stack. (i.e.,
   -- the stack will grow by 8, 16 or 32 bits)
   | PUSH8  !Word16
   | PUSH16 !Word16
   | PUSH32 !Word16

   -- Push the specified local as a 8, 16, 32 bit value onto the stack, but the
   -- value will take the whole word on the stack (i.e., the stack will grow by
   -- a word)
   -- This is useful when extracting a packed constructor field for further use.
   -- Currently we expect all values on the stack to take full words, except for
   -- the ones used for PACK (i.e., actually constructing new data types, in
   -- which case we use PUSH{8,16,32})
   | PUSH8_W  !Word16
   | PUSH16_W !Word16
   | PUSH32_W !Word16

   -- Push a ptr  (these all map to PUSH_G really)
   | PUSH_G       Name
   | PUSH_PRIMOP  PrimOp
   | PUSH_BCO     (ProtoBCO Name)

   -- Push an alt continuation
   | PUSH_ALTS          (ProtoBCO Name)
   | PUSH_ALTS_UNLIFTED (ProtoBCO Name) ArgRep
   | PUSH_ALTS_TUPLE    (ProtoBCO Name) -- continuation
                        !TupleInfo
                        (ProtoBCO Name) -- tuple return BCO

   -- Pushing 8, 16 and 32 bits of padding (for constructors).
   | PUSH_PAD8
   | PUSH_PAD16
   | PUSH_PAD32

   -- Pushing literals
   | PUSH_UBX8  Literal
   | PUSH_UBX16 Literal
   | PUSH_UBX32 Literal
   | PUSH_UBX   Literal Word16
        -- push this int/float/double/addr, on the stack. Word16
        -- is # of words to copy from literal pool.  Eitherness reflects
        -- the difficulty of dealing with MachAddr here, mostly due to
        -- the excessive (and unnecessary) restrictions imposed by the
        -- designers of the new Foreign library.  In particular it is
        -- quite impossible to convert an Addr to any other integral
        -- type, and it appears impossible to get hold of the bits of
        -- an addr, even though we need to assemble BCOs.

   -- various kinds of application
   | PUSH_APPLY_N
   | PUSH_APPLY_V
   | PUSH_APPLY_F
   | PUSH_APPLY_D
   | PUSH_APPLY_L
   | PUSH_APPLY_P
   | PUSH_APPLY_PP
   | PUSH_APPLY_PPP
   | PUSH_APPLY_PPPP
   | PUSH_APPLY_PPPPP
   | PUSH_APPLY_PPPPPP

   | SLIDE     Word16{-this many-} Word16{-down by this much-}

   -- To do with the heap
   | ALLOC_AP  !Word16 -- make an AP with this many payload words
   | ALLOC_AP_NOUPD !Word16 -- make an AP_NOUPD with this many payload words
   | ALLOC_PAP !Word16 !Word16 -- make a PAP with this arity / payload words
   | MKAP      !Word16{-ptr to AP is this far down stack-} !Word16{-number of words-}
   | MKPAP     !Word16{-ptr to PAP is this far down stack-} !Word16{-number of words-}
   | UNPACK    !Word16 -- unpack N words from t.o.s Constr
   | PACK      DataCon !Word16
                        -- after assembly, the DataCon is an index into the
                        -- itbl array
   -- For doing case trees
   | LABEL     LocalLabel
   | TESTLT_I  Int    LocalLabel
   | TESTEQ_I  Int    LocalLabel
   | TESTLT_W  Word   LocalLabel
   | TESTEQ_W  Word   LocalLabel
   | TESTLT_F  Float  LocalLabel
   | TESTEQ_F  Float  LocalLabel
   | TESTLT_D  Double LocalLabel
   | TESTEQ_D  Double LocalLabel

   -- The Word16 value is a constructor number and therefore
   -- stored in the insn stream rather than as an offset into
   -- the literal pool.
   | TESTLT_P  Word16 LocalLabel
   | TESTEQ_P  Word16 LocalLabel

   | CASEFAIL
   | JMP              LocalLabel

   -- For doing calls to C (via glue code generated by libffi)
   | CCALL            Word16    -- stack frame size
                      (RemotePtr C_ffi_cif) -- addr of the glue code
                      Word16    -- flags.
                                --
                                -- 0x1: call is interruptible
                                -- 0x2: call is unsafe
                                --
                                -- (XXX: inefficient, but I don't know
                                -- what the alignment constraints are.)

   -- For doing magic ByteArray passing to foreign calls
   | SWIZZLE          Word16 -- to the ptr N words down the stack,
                      Word16 -- add M (interpreted as a signed 16-bit entity)

   -- To Infinity And Beyond
   | ENTER
   | RETURN                 -- return a lifted value
   | RETURN_UNLIFTED ArgRep -- return an unlifted value, here's its rep
   | RETURN_TUPLE           -- return an unboxed tuple (info already on stack)

   -- Breakpoints
   | BRK_FUN          Word16 Unique (RemotePtr CostCentre)

-- -----------------------------------------------------------------------------
-- Printing bytecode instructions

instance Outputable a => Outputable (ProtoBCO a) where
   ppr (ProtoBCO { protoBCOName       = name
                 , protoBCOInstrs     = instrs
                 , protoBCOBitmap     = bitmap
                 , protoBCOBitmapSize = bsize
                 , protoBCOArity      = arity
                 , protoBCOExpr       = origin
                 , protoBCOFFIs       = ffis })
      = (text "ProtoBCO" <+> ppr name <> char '#' <> int arity
                <+> text (show ffis) <> colon)
        $$ nest 3 (case origin of
                      Left alts ->
                        vcat (zipWith (<+>) (char '{' : repeat (char ';'))
                             (map (pprStgAltShort shortStgPprOpts) alts))
                      Right rhs ->
                        pprStgRhsShort shortStgPprOpts rhs
                  )
        $$ nest 3 (text "bitmap: " <+> text (show bsize) <+> ppr bitmap)
        $$ nest 3 (vcat (map ppr instrs))

-- Print enough of the STG expression to enable the reader to find
-- the expression in the -ddump-stg output.  That is, we need to
-- include at least a binder.

pprStgExprShort :: OutputablePass pass => StgPprOpts -> GenStgExpr pass -> SDoc
pprStgExprShort _ (StgCase _expr var _ty _alts) =
  text "case of" <+> ppr var
pprStgExprShort _ (StgLet _ bnd _) =
  text "let" <+> pprStgBindShort bnd <+> text "in ..."
pprStgExprShort _ (StgLetNoEscape _ bnd _) =
  text "let-no-escape" <+> pprStgBindShort bnd <+> text "in ..."
pprStgExprShort opts (StgTick t e) = ppr t <+> pprStgExprShort opts e
pprStgExprShort opts e = pprStgExpr opts e

pprStgBindShort :: OutputablePass pass => GenStgBinding pass -> SDoc
pprStgBindShort (StgNonRec x _) =
  ppr x <+> text "= ..."
pprStgBindShort (StgRec bs) =
  char '{' <+> ppr (fst (head bs)) <+> text "= ...; ... }"

pprStgAltShort :: OutputablePass pass => StgPprOpts -> GenStgAlt pass -> SDoc
pprStgAltShort opts GenStgAlt{alt_con=con, alt_bndrs=args, alt_rhs=expr} =
  ppr con <+> sep (map ppr args) <+> text "->" <+> pprStgExprShort opts expr

pprStgRhsShort :: OutputablePass pass => StgPprOpts -> GenStgRhs pass -> SDoc
pprStgRhsShort opts (StgRhsClosure _ext _cc upd_flag args body) =
  hang (hsep [ char '\\' <> ppr upd_flag, brackets (interppSP args) ])
       4 (pprStgExprShort opts body)
pprStgRhsShort opts rhs = pprStgRhs opts rhs


instance Outputable BCInstr where
   ppr (STKCHECK n)          = text "STKCHECK" <+> ppr n
   ppr (PUSH_L offset)       = text "PUSH_L  " <+> ppr offset
   ppr (PUSH_LL o1 o2)       = text "PUSH_LL " <+> ppr o1 <+> ppr o2
   ppr (PUSH_LLL o1 o2 o3)   = text "PUSH_LLL" <+> ppr o1 <+> ppr o2 <+> ppr o3
   ppr (PUSH8  offset)       = text "PUSH8  " <+> ppr offset
   ppr (PUSH16 offset)       = text "PUSH16  " <+> ppr offset
   ppr (PUSH32 offset)       = text "PUSH32  " <+> ppr offset
   ppr (PUSH8_W  offset)     = text "PUSH8_W  " <+> ppr offset
   ppr (PUSH16_W offset)     = text "PUSH16_W  " <+> ppr offset
   ppr (PUSH32_W offset)     = text "PUSH32_W  " <+> ppr offset
   ppr (PUSH_G nm)           = text "PUSH_G  " <+> ppr nm
   ppr (PUSH_PRIMOP op)      = text "PUSH_G  " <+> text "GHC.PrimopWrappers."
                                               <> ppr op
   ppr (PUSH_BCO bco)        = hang (text "PUSH_BCO") 2 (ppr bco)

   ppr (PUSH_ALTS bco)       = hang (text "PUSH_ALTS") 2 (ppr bco)
   ppr (PUSH_ALTS_UNLIFTED bco pk) = hang (text "PUSH_ALTS_UNLIFTED" <+> ppr pk) 2 (ppr bco)
   ppr (PUSH_ALTS_TUPLE bco tuple_info tuple_bco) =
                               hang (text "PUSH_ALTS_TUPLE" <+> ppr tuple_info)
                                    2
                                    (ppr tuple_bco $+$ ppr bco)

   ppr PUSH_PAD8             = text "PUSH_PAD8"
   ppr PUSH_PAD16            = text "PUSH_PAD16"
   ppr PUSH_PAD32            = text "PUSH_PAD32"

   ppr (PUSH_UBX8  lit)      = text "PUSH_UBX8" <+> ppr lit
   ppr (PUSH_UBX16 lit)      = text "PUSH_UBX16" <+> ppr lit
   ppr (PUSH_UBX32 lit)      = text "PUSH_UBX32" <+> ppr lit
   ppr (PUSH_UBX lit nw)     = text "PUSH_UBX" <+> parens (ppr nw) <+> ppr lit
   ppr PUSH_APPLY_N          = text "PUSH_APPLY_N"
   ppr PUSH_APPLY_V          = text "PUSH_APPLY_V"
   ppr PUSH_APPLY_F          = text "PUSH_APPLY_F"
   ppr PUSH_APPLY_D          = text "PUSH_APPLY_D"
   ppr PUSH_APPLY_L          = text "PUSH_APPLY_L"
   ppr PUSH_APPLY_P          = text "PUSH_APPLY_P"
   ppr PUSH_APPLY_PP         = text "PUSH_APPLY_PP"
   ppr PUSH_APPLY_PPP        = text "PUSH_APPLY_PPP"
   ppr PUSH_APPLY_PPPP       = text "PUSH_APPLY_PPPP"
   ppr PUSH_APPLY_PPPPP      = text "PUSH_APPLY_PPPPP"
   ppr PUSH_APPLY_PPPPPP     = text "PUSH_APPLY_PPPPPP"

   ppr (SLIDE n d)           = text "SLIDE   " <+> ppr n <+> ppr d
   ppr (ALLOC_AP sz)         = text "ALLOC_AP   " <+> ppr sz
   ppr (ALLOC_AP_NOUPD sz)   = text "ALLOC_AP_NOUPD   " <+> ppr sz
   ppr (ALLOC_PAP arity sz)  = text "ALLOC_PAP   " <+> ppr arity <+> ppr sz
   ppr (MKAP offset sz)      = text "MKAP    " <+> ppr sz <+> text "words,"
                                               <+> ppr offset <+> text "stkoff"
   ppr (MKPAP offset sz)     = text "MKPAP   " <+> ppr sz <+> text "words,"
                                               <+> ppr offset <+> text "stkoff"
   ppr (UNPACK sz)           = text "UNPACK  " <+> ppr sz
   ppr (PACK dcon sz)        = text "PACK    " <+> ppr dcon <+> ppr sz
   ppr (LABEL     lab)       = text "__"       <> ppr lab <> colon
   ppr (TESTLT_I  i lab)     = text "TESTLT_I" <+> int i <+> text "__" <> ppr lab
   ppr (TESTEQ_I  i lab)     = text "TESTEQ_I" <+> int i <+> text "__" <> ppr lab
   ppr (TESTLT_W  i lab)     = text "TESTLT_W" <+> int (fromIntegral i) <+> text "__" <> ppr lab
   ppr (TESTEQ_W  i lab)     = text "TESTEQ_W" <+> int (fromIntegral i) <+> text "__" <> ppr lab
   ppr (TESTLT_F  f lab)     = text "TESTLT_F" <+> float f <+> text "__" <> ppr lab
   ppr (TESTEQ_F  f lab)     = text "TESTEQ_F" <+> float f <+> text "__" <> ppr lab
   ppr (TESTLT_D  d lab)     = text "TESTLT_D" <+> double d <+> text "__" <> ppr lab
   ppr (TESTEQ_D  d lab)     = text "TESTEQ_D" <+> double d <+> text "__" <> ppr lab
   ppr (TESTLT_P  i lab)     = text "TESTLT_P" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_P  i lab)     = text "TESTEQ_P" <+> ppr i <+> text "__" <> ppr lab
   ppr CASEFAIL              = text "CASEFAIL"
   ppr (JMP lab)             = text "JMP"      <+> ppr lab
   ppr (CCALL off marshal_addr flags) = text "CCALL   " <+> ppr off
                                                <+> text "marshal code at"
                                               <+> text (show marshal_addr)
                                               <+> (case flags of
                                                      0x1 -> text "(interruptible)"
                                                      0x2 -> text "(unsafe)"
                                                      _   -> empty)
   ppr (SWIZZLE stkoff n)    = text "SWIZZLE " <+> text "stkoff" <+> ppr stkoff
                                               <+> text "by" <+> ppr n
   ppr ENTER                 = text "ENTER"
   ppr RETURN                = text "RETURN"
   ppr (RETURN_UNLIFTED pk)  = text "RETURN_UNLIFTED  " <+> ppr pk
   ppr (RETURN_TUPLE)        = text "RETURN_TUPLE"
   ppr (BRK_FUN index uniq _cc) = text "BRK_FUN" <+> ppr index <+> ppr uniq <+> text "<cc>"



-- -----------------------------------------------------------------------------
-- The stack use, in words, of each bytecode insn.  These _must_ be
-- correct, or overestimates of reality, to be safe.

-- NOTE: we aggregate the stack use from case alternatives too, so that
-- we can do a single stack check at the beginning of a function only.

-- This could all be made more accurate by keeping track of a proper
-- stack high water mark, but it doesn't seem worth the hassle.

protoBCOStackUse :: ProtoBCO a -> Word
protoBCOStackUse bco = sum (map bciStackUse (protoBCOInstrs bco))

bciStackUse :: BCInstr -> Word
bciStackUse STKCHECK{}            = 0
bciStackUse PUSH_L{}              = 1
bciStackUse PUSH_LL{}             = 2
bciStackUse PUSH_LLL{}            = 3
bciStackUse PUSH8{}               = 1  -- overapproximation
bciStackUse PUSH16{}              = 1  -- overapproximation
bciStackUse PUSH32{}              = 1  -- overapproximation on 64bit arch
bciStackUse PUSH8_W{}             = 1  -- takes exactly 1 word
bciStackUse PUSH16_W{}            = 1  -- takes exactly 1 word
bciStackUse PUSH32_W{}            = 1  -- takes exactly 1 word
bciStackUse PUSH_G{}              = 1
bciStackUse PUSH_PRIMOP{}         = 1
bciStackUse PUSH_BCO{}            = 1
bciStackUse (PUSH_ALTS bco)       = 2 {- profiling only, restore CCCS -} +
                                    3 + protoBCOStackUse bco
bciStackUse (PUSH_ALTS_UNLIFTED bco _) = 2 {- profiling only, restore CCCS -} +
                                         4 + protoBCOStackUse bco
bciStackUse (PUSH_ALTS_TUPLE bco info _) =
   -- (tuple_bco, tuple_info word, cont_bco, stg_ctoi_t)
   -- tuple
   -- (tuple_info, tuple_bco, stg_ret_t)
   1 {- profiling only -} +
   7 + fromIntegral (tupleSize info) + protoBCOStackUse bco
bciStackUse (PUSH_PAD8)           = 1  -- overapproximation
bciStackUse (PUSH_PAD16)          = 1  -- overapproximation
bciStackUse (PUSH_PAD32)          = 1  -- overapproximation on 64bit arch
bciStackUse (PUSH_UBX8 _)         = 1  -- overapproximation
bciStackUse (PUSH_UBX16 _)        = 1  -- overapproximation
bciStackUse (PUSH_UBX32 _)        = 1  -- overapproximation on 64bit arch
bciStackUse (PUSH_UBX _ nw)       = fromIntegral nw
bciStackUse PUSH_APPLY_N{}        = 1
bciStackUse PUSH_APPLY_V{}        = 1
bciStackUse PUSH_APPLY_F{}        = 1
bciStackUse PUSH_APPLY_D{}        = 1
bciStackUse PUSH_APPLY_L{}        = 1
bciStackUse PUSH_APPLY_P{}        = 1
bciStackUse PUSH_APPLY_PP{}       = 1
bciStackUse PUSH_APPLY_PPP{}      = 1
bciStackUse PUSH_APPLY_PPPP{}     = 1
bciStackUse PUSH_APPLY_PPPPP{}    = 1
bciStackUse PUSH_APPLY_PPPPPP{}   = 1
bciStackUse ALLOC_AP{}            = 1
bciStackUse ALLOC_AP_NOUPD{}      = 1
bciStackUse ALLOC_PAP{}           = 1
bciStackUse (UNPACK sz)           = fromIntegral sz
bciStackUse LABEL{}               = 0
bciStackUse TESTLT_I{}            = 0
bciStackUse TESTEQ_I{}            = 0
bciStackUse TESTLT_W{}            = 0
bciStackUse TESTEQ_W{}            = 0
bciStackUse TESTLT_F{}            = 0
bciStackUse TESTEQ_F{}            = 0
bciStackUse TESTLT_D{}            = 0
bciStackUse TESTEQ_D{}            = 0
bciStackUse TESTLT_P{}            = 0
bciStackUse TESTEQ_P{}            = 0
bciStackUse CASEFAIL{}            = 0
bciStackUse JMP{}                 = 0
bciStackUse ENTER{}               = 0
bciStackUse RETURN{}              = 0
bciStackUse RETURN_UNLIFTED{}     = 1 -- pushes stg_ret_X for some X
bciStackUse RETURN_TUPLE{}        = 1 -- pushes stg_ret_t header
bciStackUse CCALL{}               = 0
bciStackUse SWIZZLE{}             = 0
bciStackUse BRK_FUN{}             = 0

-- These insns actually reduce stack use, but we need the high-tide level,
-- so can't use this info.  Not that it matters much.
bciStackUse SLIDE{}               = 0
bciStackUse MKAP{}                = 0
bciStackUse MKPAP{}               = 0
bciStackUse PACK{}                = 1 -- worst case is PACK 0 words
