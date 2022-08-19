/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2017-2021
 *
 * IPE API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

typedef struct InfoProv_ {
    const char *table_name;
    const char *closure_desc;
    const char *ty_desc;
    const char *label;
    const char *module;
    const char *src_file;
    const char *src_span;
} InfoProv;

typedef struct InfoProvEnt_ {
    const StgInfoTable *info;
    InfoProv prov;
} InfoProvEnt;


/*
 * On-disk representation
 */

/*
 * A byte offset into the string table.
 * We use offsets rather than pointers as:
 *
 *  a. they are smaller than pointers on 64-bit platforms
 *  b. they are easier on the linker since they do not need
 *     to be relocated
 */
typedef uint32_t StringIdx;

// This is the provenance representation that we emit to
// object code (see
// GHC.GHC.StgToCmm.InfoTableProv.emitIpeBufferListNode).
//
// The size of this must be a multiple of the word size
// to ensure correct packing.
typedef struct {
    const StgInfoTable *info;
    StringIdx table_name;
    StringIdx closure_desc;
    StringIdx ty_desc;
    StringIdx label;
    StringIdx module_name;
    StringIdx src_file;
    StringIdx src_span;
    uint32_t _padding;
} IpeBufferEntry;

GHC_STATIC_ASSERT(sizeof(IpeBufferEntry) % (WORD_SIZE_IN_BITS / 8) == 0, "sizeof(IpeBufferEntry) must be a multiple of the word size");

typedef struct IpeBufferListNode_ {
    struct IpeBufferListNode_ *next;
    // Everything below is read-only and generated by the codegen
    const char *string_table;
    StgWord count;
    IpeBufferEntry entries[];
} IpeBufferListNode;

void registerInfoProvList(IpeBufferListNode *node);
InfoProvEnt *lookupIPE(const StgInfoTable *info);
