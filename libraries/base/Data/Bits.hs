{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

module Data.Bits (
  Bits(
    (.&.), (.|.), xor,
    complement,
    shift,
    rotate,
    zeroBits,
    bit,
    setBit,
    clearBit,
    complementBit,
    testBit,
    bitSizeMaybe,
    bitSize,
    isSigned,
    shiftL, shiftR,
    unsafeShiftL, unsafeShiftR,
    rotateL, rotateR,
    popCount
  ),
  FiniteBits(
    finiteBitSize,
    countLeadingZeros,
    countTrailingZeros
  ),
  bitDefault,
  testBitDefault,
  popCountDefault,
  toIntegralSized,
  oneBits,
  (.^.),
  (.>>.), (.<<.), (!>>.), (!<<.),
  And(..), Ior(..), Xor(..), Iff(..)
 ) where

import GHC.Base
import GHC.Bits
import GHC.Enum
import qualified GHC.List as List
import GHC.Read
import GHC.Show

oneBits :: (FiniteBits a) => a
oneBits = complement zeroBits
{-# INLINE oneBits #-}

(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 6 .^.

(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 .<<.

(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR

infixl 8 !>>.

(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL

infixl 8 !<<.

newtype And a = And { getAnd :: a }
  deriving newtype (
                    Bounded,
                    Enum,
                    Bits,
                    FiniteBits,
                    Eq
                    )
  deriving stock (
                  Show,
                  Read
                 )

instance (Bits a) => Semigroup (And a) where
  And x <> And y = And (x .&. y)

instance (FiniteBits a) => Monoid (And a) where
  mempty = And oneBits
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

newtype Ior a = Ior { getIor :: a }
  deriving newtype (
                    Bounded,
                    Enum,
                    Bits,
                    FiniteBits,
                    Eq
                    )
  deriving stock (
                  Show,
                  Read
                 )

instance (Bits a) => Semigroup (Ior a) where
  Ior x <> Ior y = Ior (x .|. y)

instance (Bits a) => Monoid (Ior a) where
  mempty = Ior zeroBits
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

newtype Xor a = Xor { getXor :: a }
  deriving newtype (
                    Bounded,
                    Enum,
                    Bits,
                    FiniteBits,
                    Eq
                    )
  deriving stock (
                  Show,
                  Read
                 )

instance (Bits a) => Semigroup (Xor a) where
  Xor x <> Xor y = Xor (x `xor` y)

instance (Bits a) => Monoid (Xor a) where
  mempty = Xor zeroBits
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}

newtype Iff a = Iff { getIff :: a }
  deriving newtype (
                    Bounded,
                    Enum,
                    Bits,
                    FiniteBits,
                    Eq
                    )
  deriving stock (
                  Show,
                  Read
                 )

instance (FiniteBits a) => Semigroup (Iff a) where
  Iff x <> Iff y = Iff . complement $ (x `xor` y)

instance (FiniteBits a) => Monoid (Iff a) where
  mempty = Iff oneBits
  mconcat = List.foldl' (<>) mempty
  {-# INLINE mconcat #-}
