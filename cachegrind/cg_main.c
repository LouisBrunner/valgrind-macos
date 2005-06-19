
/*--------------------------------------------------------------------*/
/*--- Cachegrind: everything but the simulation itself.            ---*/
/*---                                                    cg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2005 Nicholas Nethercote
      njn@valgrind.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_profile.h"
#include "pub_tool_tooliface.h"

#include "cg_arch.h"
#include "cg_sim.c"

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

#define MIN_LINE_SIZE         16
#define FILE_LEN              256
#define FN_LEN                256

/*------------------------------------------------------------*/
/*--- Profiling events                                     ---*/
/*------------------------------------------------------------*/

typedef 
   enum { 
      VgpGetLineCC = VgpFini+1,
      VgpCacheSimulate,
      VgpCacheResults
   } 
   VgpToolCC;

/*------------------------------------------------------------*/
/*--- Types and Data Structures                            ---*/
/*------------------------------------------------------------*/

typedef struct _CC CC;
struct _CC {
   ULong a;
   ULong m1;
   ULong m2;
};

//------------------------------------------------------------
// Primary data structure #1: CC table
// - Holds the per-source-line hit/miss stats, grouped by file/function/line.
// - hash(file, hash(fn, hash(line+CC)))
// - Each hash table is separately chained.
// - The array sizes below work fairly well for Konqueror.
// - Lookups done by instr_addr, which is converted immediately to a source
//   location.
// - Traversed for dumping stats at end in file/func/line hierarchy.

#define N_FILE_ENTRIES        251
#define   N_FN_ENTRIES         53
#define N_LINE_ENTRIES         37

typedef struct _lineCC lineCC;
struct _lineCC {
   Int      line;
   CC       Ir;
   CC       Dr;
   CC       Dw;
   lineCC*  next;
};

typedef struct _fnCC fnCC;
struct _fnCC {
   Char*   fn;
   fnCC*   next;
   lineCC* lines[N_LINE_ENTRIES];
};

typedef struct _fileCC fileCC;
struct _fileCC {
   Char*   file;
   fileCC* next;
   fnCC*   fns[N_FN_ENTRIES];
};

// Top level of CC table.  Auto-zeroed.
static fileCC *CC_table[N_FILE_ENTRIES];

//------------------------------------------------------------
// Primary data structre #2: Instr-info table
// - Holds the cached info about each instr that is used for simulation.
// - table(BB_start_addr, list(instr_info))
// - For each BB, each instr_info in the list holds info about the
//   instruction (instr_len, instr_addr, etc), plus a pointer to its line
//   CC.  This node is what's passed to the simulation function.
// - When BBs are discarded the relevant list(instr_details) is freed.

typedef struct _instr_info instr_info;
struct _instr_info {
   Addr    instr_addr;
   UChar   instr_len;
   UChar   data_size;
   lineCC* parent;       // parent line-CC
};

typedef struct _BB_info BB_info;
struct _BB_info {
   BB_info*   next;              // next field
   Addr       BB_addr;           // key
   Int        n_instrs;
   instr_info instrs[0];
};

VgHashTable instr_info_table;    // hash(Addr, BB_info)

//------------------------------------------------------------
// Stats
static Int  distinct_files      = 0;
static Int  distinct_fns        = 0;
static Int  distinct_lines      = 0;
static Int  distinct_instrs     = 0;

static Int  full_debug_BBs      = 0;
static Int  file_line_debug_BBs = 0;
static Int  fn_debug_BBs        = 0;
static Int  no_debug_BBs        = 0;

static Int  BB_retranslations   = 0;

/*------------------------------------------------------------*/
/*--- CC table operations                                  ---*/
/*------------------------------------------------------------*/

static void get_debug_info(Addr instr_addr, Char file[FILE_LEN],
                           Char fn[FN_LEN], Int* line)
{
   Bool found_file_line = VG_(get_filename_linenum)(
                             instr_addr, 
                             file, FILE_LEN,
                             NULL, 0, NULL,
                             line
                          );
   Bool found_fn        = VG_(get_fnname)(instr_addr, fn, FN_LEN);

   if (!found_file_line) {
      VG_(strcpy)(file, "???");
      *line = 0;
   }
   if (!found_fn) {
      VG_(strcpy)(fn,  "???");
   }
   if (found_file_line) {
      if (found_fn) full_debug_BBs++;
      else          file_line_debug_BBs++;
   } else {
      if (found_fn) fn_debug_BBs++;
      else          no_debug_BBs++;
   }
}

static UInt hash(Char *s, UInt table_size)
{
   const int hash_constant = 256;
   int hash_value = 0;
   for ( ; *s; s++)
      hash_value = (hash_constant * hash_value + *s) % table_size;
   return hash_value;
}

static __inline__ 
fileCC* new_fileCC(Char filename[], fileCC* next)
{
   // Using calloc() zeroes the fns[] array
   fileCC* cc = VG_(calloc)(1, sizeof(fileCC));
   cc->file   = VG_(strdup)(filename);
   cc->next   = next;
   return cc;
}

static __inline__ 
fnCC* new_fnCC(Char fn[], fnCC* next)
{
   // Using calloc() zeroes the lines[] array
   fnCC* cc = VG_(calloc)(1, sizeof(fnCC));
   cc->fn   = VG_(strdup)(fn);
   cc->next = next;
   return cc;
}

static __inline__ 
lineCC* new_lineCC(Int line, lineCC* next)
{
   // Using calloc() zeroes the Ir/Dr/Dw CCs and the instrs[] array
   lineCC* cc = VG_(calloc)(1, sizeof(lineCC));
   cc->line   = line;
   cc->next   = next;
   return cc;
}

static __inline__ 
instr_info* new_instr_info(Addr instr_addr, lineCC* parent, instr_info* next)
{
   // Using calloc() zeroes instr_len and data_size
   instr_info* ii = VG_(calloc)(1, sizeof(instr_info));
   ii->instr_addr = instr_addr;
   ii->parent     = parent; 
   return ii;
}

// Do a three step traversal: by file, then fn, then line.
// In all cases prepends new nodes to their chain.  Returns a pointer to the
// line node, creates a new one if necessary.
static lineCC* get_lineCC(Addr origAddr)
{
   fileCC *curr_fileCC;
   fnCC   *curr_fnCC;
   lineCC *curr_lineCC;
   Char    file[FILE_LEN], fn[FN_LEN];
   Int     line;
   UInt    file_hash, fn_hash, line_hash;

   get_debug_info(origAddr, file, fn, &line);

   VGP_PUSHCC(VgpGetLineCC);

   // level 1
   file_hash = hash(file, N_FILE_ENTRIES);
   curr_fileCC   = CC_table[file_hash];
   while (NULL != curr_fileCC && !VG_STREQ(file, curr_fileCC->file)) {
      curr_fileCC = curr_fileCC->next;
   }
   if (NULL == curr_fileCC) {
      CC_table[file_hash] = curr_fileCC = 
         new_fileCC(file, CC_table[file_hash]);
      distinct_files++;
   }

   // level 2
   fn_hash = hash(fn, N_FN_ENTRIES);
   curr_fnCC   = curr_fileCC->fns[fn_hash];
   while (NULL != curr_fnCC && !VG_STREQ(fn, curr_fnCC->fn)) {
      curr_fnCC = curr_fnCC->next;
   }
   if (NULL == curr_fnCC) {
      curr_fileCC->fns[fn_hash] = curr_fnCC = 
         new_fnCC(fn, curr_fileCC->fns[fn_hash]);
      distinct_fns++;
   }

   // level 3
   line_hash   = line % N_LINE_ENTRIES;
   curr_lineCC = curr_fnCC->lines[line_hash];
   while (NULL != curr_lineCC && line != curr_lineCC->line) {
      curr_lineCC = curr_lineCC->next;
   }
   if (NULL == curr_lineCC) {
      curr_fnCC->lines[line_hash] = curr_lineCC = 
         new_lineCC(line, curr_fnCC->lines[line_hash]);
      distinct_lines++;
   }

   VGP_POPCC(VgpGetLineCC);
   return curr_lineCC;
}

/*------------------------------------------------------------*/
/*--- Cache simulation functions                           ---*/
/*------------------------------------------------------------*/

static VGA_REGPARM(1)
void log_1I_0D_cache_access(instr_info* n)
{
   //VG_(printf)("1I_0D : CCaddr=0x%x, iaddr=0x%x, isize=%u\n",
   //            n, n->instr_addr, n->instr_len);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VGA_REGPARM(2)
void log_1I_1Dr_cache_access(instr_info* n, Addr data_addr)
{
   //VG_(printf)("1I_1Dr: CCaddr=%p, iaddr=%p, isize=%u, daddr=%p, dsize=%u\n",
   //            n, n->instr_addr, n->instr_len, data_addr, n->data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, n->data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.m2);
   n->parent->Dr.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VGA_REGPARM(2)
void log_1I_1Dw_cache_access(instr_info* n, Addr data_addr)
{
   //VG_(printf)("1I_1Dw: CCaddr=%p, iaddr=%p, isize=%u, daddr=%p, dsize=%u\n",
   //            n, n->instr_addr, n->instr_len, data_addr, n->data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, n->data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.m2);
   n->parent->Dw.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VGA_REGPARM(3)
void log_1I_2D_cache_access(instr_info* n, Addr data_addr1, Addr data_addr2)
{
   //VG_(printf)("1I_2D: CCaddr=%p, iaddr=%p, isize=%u, daddr1=%p, daddr2=%p, dsize=%u\n",
   //            n, n->instr_addr, n->instr_len, data_addr1, data_addr2, n->data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len,
                     &n->parent->Ir.m1,  &n->parent->Ir.m2);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr1, n->data_size,
                     &n->parent->Dr.m1, &n->parent->Dr.m2);
   n->parent->Dr.a++;
   cachesim_D1_doref(data_addr2, n->data_size,
                     &n->parent->Dw.m1, &n->parent->Dw.m2);
   n->parent->Dw.a++;
   VGP_POPCC(VgpCacheSimulate);
}

/*------------------------------------------------------------*/
/*--- Instrumentation                                      ---*/
/*------------------------------------------------------------*/

static
BB_info* get_BB_info(IRBB* bbIn, Addr origAddr, Bool* bbSeenBefore)
{
   Int          i, n_instrs;
   IRStmt*      st;
   BB_info*     bbInfo;
   VgHashNode** dummy;
   
   // Count number of original instrs in BB
   n_instrs = 0;
   for (i = 0; i < bbIn->stmts_used; i++) {
      st = bbIn->stmts[i];
      if (Ist_IMark == st->tag) n_instrs++;
   }

   // Get the BB_info
   bbInfo = (BB_info*)VG_(HT_get_node)(instr_info_table, origAddr, &dummy);
   *bbSeenBefore = ( NULL == bbInfo ? False : True );
   if (*bbSeenBefore) {
      // BB must have been translated before, but flushed from the TT
      tl_assert(bbInfo->n_instrs == n_instrs );
      BB_retranslations++;
   } else {
      // BB never translated before (at this address, at least;  could have
      // been unloaded and then reloaded elsewhere in memory)
      bbInfo = VG_(calloc)(1, sizeof(BB_info) + n_instrs*sizeof(instr_info)); 
      bbInfo->BB_addr = origAddr;
      bbInfo->n_instrs = n_instrs;
      VG_(HT_add_node)( instr_info_table, (VgHashNode*)bbInfo );
      distinct_instrs++;
   }
   return bbInfo;
}

static 
void handleOneStatement(IRTypeEnv* tyenv, IRBB* bbOut, IRStmt* st,
                        Addr* instrAddr, UInt* instrLen,
                        IRExpr** loadAddrExpr, IRExpr** storeAddrExpr,
                        UInt* dataSize)
{
   tl_assert(isFlatIRStmt(st));

   switch (st->tag) {
   case Ist_NoOp:
      break;

   case Ist_AbiHint:
      /* ABI hints aren't interesting to cachegrind.  Ignore. */
      break;

   case Ist_IMark:
      /* st->Ist.IMark.addr is a 64-bit int.  ULong_to_Ptr casts this
         to the host's native pointer type; if that is 32 bits then it
         discards the upper 32 bits.  If we are cachegrinding on a
         32-bit host then we are also ensured that the guest word size
         is 32 bits, due to the assertion in cg_instrument that the
         host and guest word sizes must be the same.  Hence
         st->Ist.IMark.addr will have been derived from a 32-bit guest
         code address and truncation of it is safe.  I believe this
         assignment should be correct for both 32- and 64-bit
         machines. */
      *instrAddr = (Addr)ULong_to_Ptr(st->Ist.IMark.addr);
      *instrLen =        st->Ist.IMark.len;
      addStmtToIRBB( bbOut, st );
      break;

   case Ist_Tmp: {
      IRExpr* data = st->Ist.Tmp.data;
      if (data->tag == Iex_LDle) {
         IRExpr* aexpr = data->Iex.LDle.addr;
         tl_assert( isIRAtom(aexpr) );

         // XXX: repe cmpsb does two loads... the first one is ignored here!
         //tl_assert( NULL == *loadAddrExpr );          // XXX: ???
         *loadAddrExpr = aexpr;
         *dataSize = sizeofIRType(data->Iex.LDle.ty);
      }
      addStmtToIRBB( bbOut, st );
      break;
   }
      
   case Ist_STle: {
      IRExpr* data  = st->Ist.STle.data;
      IRExpr* aexpr = st->Ist.STle.addr;
      tl_assert( isIRAtom(aexpr) );
      tl_assert( NULL == *storeAddrExpr );          // XXX: ???
      *storeAddrExpr = aexpr;
      *dataSize = sizeofIRType(typeOfIRExpr(tyenv, data));
      addStmtToIRBB( bbOut, st );
      break;
   }
   
   case Ist_Dirty: {
      IRDirty* d = st->Ist.Dirty.details;
      if (d->mFx != Ifx_None) {
         /* This dirty helper accesses memory.  Collect the
            details. */
         tl_assert(d->mAddr != NULL);
         tl_assert(d->mSize != 0);
         *dataSize = d->mSize;
         if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
            *loadAddrExpr = d->mAddr;
         if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
            *storeAddrExpr = d->mAddr;
      } else {
         tl_assert(d->mAddr == NULL);
         tl_assert(d->mSize == 0);
      }
      addStmtToIRBB( bbOut, st );
      break;
   }

   case Ist_Put:
   case Ist_PutI:
   case Ist_Exit:
   case Ist_MFence:
      addStmtToIRBB( bbOut, st );
      break;

   default:
      VG_(printf)("\n");
      ppIRStmt(st);
      VG_(printf)("\n");
      VG_(tool_panic)("Cachegrind: unhandled IRStmt");
   }
}

static
void do_details( instr_info* n, Bool bbSeenBefore,
                 Addr instr_addr, Int instr_len, Int data_size )
{
   if (bbSeenBefore) {
      tl_assert( n->instr_addr == instr_addr );
      tl_assert( n->instr_len  == instr_len );
      tl_assert( n->data_size  == data_size );
      // Don't check that (n->parent == parent)... it's conceivable that
      // the debug info might change;  the other asserts should be enough to
      // detect anything strange.
   } else {
      lineCC* parent = get_lineCC(instr_addr);
      n->instr_addr = instr_addr;
      n->instr_len  = instr_len;
      n->data_size  = data_size;
      n->parent     = parent;
   }
}

static Bool loadStoreAddrsMatch(IRExpr* loadAddrExpr, IRExpr* storeAddrExpr)
{
  // I'm assuming that for 'modify' instructions, that Vex always makes
  // the loadAddrExpr and storeAddrExpr be of the same type, ie. both Tmp
  // expressions, or both Const expressions.
  tl_assert(isIRAtom(loadAddrExpr));
  tl_assert(isIRAtom(storeAddrExpr));
  return eqIRAtom(loadAddrExpr, storeAddrExpr);
}

// Instrumentation for the end of each original instruction.
static
void endOfInstr(IRBB* bbOut, instr_info* i_node, Bool bbSeenBefore,
                UInt instrAddr, UInt instrLen, UInt dataSize,
                IRExpr* loadAddrExpr, IRExpr* storeAddrExpr)
{
   IRDirty* di;
   IRExpr  *arg1, *arg2, *arg3, **argv;
   Int      argc;
   Char*    helperName;
   void*    helperAddr;
   IRType   wordTy;

   // Stay sane ...
   tl_assert(sizeof(HWord) == sizeof(void*));
   if (sizeof(HWord) == 4) {
      wordTy = Ity_I32;
   } else
   if (sizeof(HWord) == 8) {
      wordTy = Ity_I64;
   } else {
      VG_(tool_panic)("endOfInstr: strange word size");
   }

   if (loadAddrExpr) 
      tl_assert(wordTy == typeOfIRExpr(bbOut->tyenv, loadAddrExpr));
   if (storeAddrExpr) 
      tl_assert(wordTy == typeOfIRExpr(bbOut->tyenv, storeAddrExpr));


   // Nb: instrLen will be zero if Vex failed to decode it.
   tl_assert( 0 == instrLen ||
              (instrLen >= VGA_MIN_INSTR_SZB && 
               instrLen <= VGA_MAX_INSTR_SZB) );

   // Large (eg. 28B, 108B, 512B on x86) data-sized instructions will be
   // done inaccurately, but they're very rare and this avoids errors from
   // hitting more than two cache lines in the simulation.
   if (dataSize > MIN_LINE_SIZE) dataSize = MIN_LINE_SIZE;

   // Setup 1st arg: instr_info node's address
   // Believed to be 64-bit clean
   do_details(i_node, bbSeenBefore, instrAddr, instrLen, dataSize );
   arg1 = mkIRExpr_HWord( (HWord)i_node );

   if (!loadAddrExpr && !storeAddrExpr) {
      // no load/store
      tl_assert(0 == dataSize);
      helperName = "log_1I_0D_cache_access";
      helperAddr = &log_1I_0D_cache_access;
      argc = 1;
      argv = mkIRExprVec_1(arg1);

   } else if (loadAddrExpr && !storeAddrExpr) {
      // load
      tl_assert( isIRAtom(loadAddrExpr) );
      helperName = "log_1I_1Dr_cache_access";
      helperAddr = &log_1I_1Dr_cache_access;
      argc = 2;
      arg2 = loadAddrExpr;
      argv = mkIRExprVec_2(arg1, arg2);

   } else if (!loadAddrExpr && storeAddrExpr) {
      // store
      tl_assert( isIRAtom(storeAddrExpr) );
      helperName = "log_1I_1Dw_cache_access";
      helperAddr = &log_1I_1Dw_cache_access;
      argc = 2;
      arg2 = storeAddrExpr;
      argv = mkIRExprVec_2(arg1, arg2);
 
   } else {
      tl_assert( loadAddrExpr && storeAddrExpr );
      tl_assert( isIRAtom(loadAddrExpr) );
      tl_assert( isIRAtom(storeAddrExpr) );

      if ( loadStoreAddrsMatch(loadAddrExpr, storeAddrExpr) ) {
         // modify
         helperName = "log_1I_1Dr_cache_access";
         helperAddr = &log_1I_1Dr_cache_access;
         argc = 2;
         arg2 = loadAddrExpr;
         argv = mkIRExprVec_2(arg1, arg2);

      } else {
         // load/store
         helperName = "log_1I_2D_cache_access";
         helperAddr = &log_1I_2D_cache_access;
         argc = 3;
         arg2 = loadAddrExpr;
         arg3 = storeAddrExpr;
         argv = mkIRExprVec_3(arg1, arg2, arg3);
      }
   }

   // Add call to the instrumentation function
   di = unsafeIRDirty_0_N( argc, helperName, helperAddr, argv);
   addStmtToIRBB( bbOut, IRStmt_Dirty(di) );
}

static IRBB* cg_instrument ( IRBB* bbIn, VexGuestLayout* layout, 
                             IRType gWordTy, IRType hWordTy )
{
   Int      i, dataSize = 0, bbInfo_i;
   IRBB*    bbOut;
   IRStmt*  st;
   BB_info* bbInfo;
   Bool     bbSeenBefore = False;
   Addr     instrAddr, origAddr;
   UInt     instrLen;
   IRExpr  *loadAddrExpr, *storeAddrExpr;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Set up BB */
   bbOut           = emptyIRBB();
   bbOut->tyenv    = dopyIRTypeEnv(bbIn->tyenv);
   bbOut->next     = dopyIRExpr(bbIn->next);
   bbOut->jumpkind = bbIn->jumpkind;

   // Get the first statement, and origAddr from it
   i = 0;
   tl_assert(bbIn->stmts_used > 0);
   st = bbIn->stmts[0];
   tl_assert(Ist_IMark == st->tag);
   origAddr = (Addr)st->Ist.IMark.addr;
   tl_assert(origAddr == st->Ist.IMark.addr);  // XXX: check no overflow

   // Get block info
   bbInfo = get_BB_info(bbIn, origAddr, &bbSeenBefore);
   bbInfo_i = 0;

   do {
      // We should be at an IMark statement
      tl_assert(Ist_IMark == st->tag);

      // Reset stuff for this original instruction
      loadAddrExpr = storeAddrExpr = NULL;
      dataSize = 0;

      // Process all the statements for this original instruction (ie. until
      // the next IMark statement, or the end of the block)
      do {
         handleOneStatement(bbIn->tyenv, bbOut, st, &instrAddr, &instrLen,
                            &loadAddrExpr, &storeAddrExpr, &dataSize);
         i++;
         st = ( i < bbIn->stmts_used ? bbIn->stmts[i] : NULL );
      } 
      while (st && Ist_IMark != st->tag);

      // Add instrumentation for this original instruction.
      endOfInstr(bbOut, &bbInfo->instrs[ bbInfo_i ], bbSeenBefore,
                 instrAddr, instrLen, dataSize, loadAddrExpr, storeAddrExpr);

      bbInfo_i++;
   } 
   while (st);

   return bbOut;
}

/*------------------------------------------------------------*/
/*--- Cache configuration                                  ---*/
/*------------------------------------------------------------*/

#define UNDEFINED_CACHE     { -1, -1, -1 }

static cache_t clo_I1_cache = UNDEFINED_CACHE;
static cache_t clo_D1_cache = UNDEFINED_CACHE;
static cache_t clo_L2_cache = UNDEFINED_CACHE;

/* Checks cache config is ok;  makes it so if not. */
static 
void check_cache(cache_t* cache, Char *name)
{
   /* First check they're all powers of two */
   if (-1 == VG_(log2)(cache->size)) {
      VG_(message)(Vg_UserMsg,
         "error: %s size of %dB not a power of two; aborting.",
         name, cache->size);
      VG_(exit)(1);
   }

   if (-1 == VG_(log2)(cache->assoc)) {
      VG_(message)(Vg_UserMsg,
         "error: %s associativity of %d not a power of two; aborting.",
         name, cache->assoc);
      VG_(exit)(1);
   }

   if (-1 == VG_(log2)(cache->line_size)) {
      VG_(message)(Vg_UserMsg,
         "error: %s line size of %dB not a power of two; aborting.",
         name, cache->line_size);
      VG_(exit)(1);
   }

   // Then check line size >= 16 -- any smaller and a single instruction could
   // straddle three cache lines, which breaks a simulation assertion and is
   // stupid anyway.
   if (cache->line_size < MIN_LINE_SIZE) {
      VG_(message)(Vg_UserMsg,
         "error: %s line size of %dB too small; aborting.", 
         name, cache->line_size);
      VG_(exit)(1);
   }

   /* Then check cache size > line size (causes seg faults if not). */
   if (cache->size <= cache->line_size) {
      VG_(message)(Vg_UserMsg,
         "error: %s cache size of %dB <= line size of %dB; aborting.",
         name, cache->size, cache->line_size);
      VG_(exit)(1);
   }

   /* Then check assoc <= (size / line size) (seg faults otherwise). */
   if (cache->assoc > (cache->size / cache->line_size)) {
      VG_(message)(Vg_UserMsg,
         "warning: %s associativity > (size / line size); aborting.", name);
      VG_(exit)(1);
   }
}

static 
void configure_caches(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
#define DEFINED(L)   (-1 != L.size  || -1 != L.assoc || -1 != L.line_size)

   Int n_clos = 0;

   // Count how many were defined on the command line.
   if (DEFINED(clo_I1_cache)) { n_clos++; }
   if (DEFINED(clo_D1_cache)) { n_clos++; }
   if (DEFINED(clo_L2_cache)) { n_clos++; }

   // Set the cache config (using auto-detection, if supported by the
   // architecture)
   VGA_(configure_caches)( I1c, D1c, L2c, (3 == n_clos) );

   // Then replace with any defined on the command line.
   if (DEFINED(clo_I1_cache)) { *I1c = clo_I1_cache; }
   if (DEFINED(clo_D1_cache)) { *D1c = clo_D1_cache; }
   if (DEFINED(clo_L2_cache)) { *L2c = clo_L2_cache; }

   // Then check values and fix if not acceptable.
   check_cache(I1c, "I1");
   check_cache(D1c, "D1");
   check_cache(L2c, "L2");

   if (VG_(clo_verbosity) > 1) {
      VG_(message)(Vg_UserMsg, "Cache configuration used:");
      VG_(message)(Vg_UserMsg, "  I1: %dB, %d-way, %dB lines",
                               I1c->size, I1c->assoc, I1c->line_size);
      VG_(message)(Vg_UserMsg, "  D1: %dB, %d-way, %dB lines",
                               D1c->size, D1c->assoc, D1c->line_size);
      VG_(message)(Vg_UserMsg, "  L2: %dB, %d-way, %dB lines",
                               L2c->size, L2c->assoc, L2c->line_size);
   }
#undef CMD_LINE_DEFINED
}

/*------------------------------------------------------------*/
/*--- cg_fini() and related function                       ---*/
/*------------------------------------------------------------*/

// Total reads/writes/misses.  Calculated during CC traversal at the end.
// All auto-zeroed.
static CC Ir_total;
static CC Dr_total;
static CC Dw_total;

static Char* cachegrind_out_file;

static void fprint_lineCC(Int fd, lineCC* n)
{
   Char buf[512];
   VG_(sprintf)(buf, "%u %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
                      n->line,
                      n->Ir.a, n->Ir.m1, n->Ir.m2, 
                      n->Dr.a, n->Dr.m1, n->Dr.m2,
                      n->Dw.a, n->Dw.m1, n->Dw.m2);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   Ir_total.a += n->Ir.a;  Ir_total.m1 += n->Ir.m1;  Ir_total.m2 += n->Ir.m2;
   Dr_total.a += n->Dr.a;  Dr_total.m1 += n->Dr.m1;  Dr_total.m2 += n->Dr.m2;
   Dw_total.a += n->Dw.a;  Dw_total.m1 += n->Dw.m1;  Dw_total.m2 += n->Dw.m2;
}

static void fprint_CC_table_and_calc_totals(void)
{
   Int     fd;
   Char    buf[512];
   fileCC *curr_fileCC;
   fnCC   *curr_fnCC;
   lineCC *curr_lineCC;
   Int     i, j, k;

   VGP_PUSHCC(VgpCacheResults);

   fd = VG_(open)(cachegrind_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                                       VKI_S_IRUSR|VKI_S_IWUSR);
   if (fd < 0) {
      // If the file can't be opened for whatever reason (conflict
      // between multiple cachegrinded processes?), give up now.
      VG_(message)(Vg_UserMsg,
         "error: can't open cache simulation output file '%s'",
         cachegrind_out_file );
      VG_(message)(Vg_UserMsg,
         "       ... so simulation results will be missing.");
      return;
   }

   // "desc:" lines (giving I1/D1/L2 cache configuration).  The spaces after
   // the 2nd colon makes cg_annotate's output look nicer.
   VG_(sprintf)(buf, "desc: I1 cache:         %s\n"
                     "desc: D1 cache:         %s\n"
                     "desc: L2 cache:         %s\n",
                     I1.desc_line, D1.desc_line, L2.desc_line);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   // "cmd:" line
   VG_(strcpy)(buf, "cmd:");
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   for (i = 0; i < VG_(client_argc); i++) {
       if (VG_(client_argv)[i] == NULL)
          continue;
       VG_(write)(fd, " ", 1);
       VG_(write)(fd, VG_(client_argv)[i], VG_(strlen)(VG_(client_argv)[i]));
   }
   // "events:" line
   VG_(sprintf)(buf, "\nevents: Ir I1mr I2mr Dr D1mr D2mr Dw D1mw D2mw\n");
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   // Six loops here:  three for the hash table arrays, and three for the
   // chains hanging off the hash table arrays.
   for (i = 0; i < N_FILE_ENTRIES; i++) {
      curr_fileCC = CC_table[i];
      while (curr_fileCC != NULL) {
         VG_(sprintf)(buf, "fl=%s\n", curr_fileCC->file);
         VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

         for (j = 0; j < N_FN_ENTRIES; j++) {
            curr_fnCC = curr_fileCC->fns[j];
            while (curr_fnCC != NULL) {
               VG_(sprintf)(buf, "fn=%s\n", curr_fnCC->fn);
               VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

               for (k = 0; k < N_LINE_ENTRIES; k++) {
                  curr_lineCC = curr_fnCC->lines[k];
                  while (curr_lineCC != NULL) {
                     fprint_lineCC(fd, curr_lineCC);
                     curr_lineCC = curr_lineCC->next;
                  }
               }
               curr_fnCC = curr_fnCC->next;
            }
         }
         curr_fileCC = curr_fileCC->next;
      }
   }

   // Summary stats must come after rest of table, since we calculate them
   // during traversal.  */ 
   VG_(sprintf)(buf, "summary: "
                     "%llu %llu %llu %llu %llu %llu %llu %llu %llu\n", 
                     Ir_total.a, Ir_total.m1, Ir_total.m2,
                     Dr_total.a, Dr_total.m1, Dr_total.m2,
                     Dw_total.a, Dw_total.m1, Dw_total.m2);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   VG_(close)(fd);
}

static UInt ULong_width(ULong n)
{
   UInt w = 0;
   while (n > 0) {
      n = n / 10;
      w++;
   }
   return w + (w-1)/3;   // add space for commas
}

static
void percentify(Int n, Int ex, Int field_width, char buf[]) 
{
   int i, len, space;
    
   VG_(sprintf)(buf, "%d.%d%%", n / ex, n % ex);
   len = VG_(strlen)(buf);
   space = field_width - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}

static void cg_fini(Int exitcode)
{
   static char buf1[128], buf2[128], buf3[128], fmt [128];

   CC D_total;
   ULong L2_total_m, L2_total_mr, L2_total_mw,
         L2_total, L2_total_r, L2_total_w;
   Int l1, l2, l3;
   Int p;

   fprint_CC_table_and_calc_totals();

   if (VG_(clo_verbosity) == 0) 
      return;

   /* I cache results.  Use the I_refs value to determine the first column
    * width. */
   l1 = ULong_width(Ir_total.a);
   l2 = ULong_width(Dr_total.a);
   l3 = ULong_width(Dw_total.a);

   /* Make format string, getting width right for numbers */
   VG_(sprintf)(fmt, "%%s %%,%dld", l1);
   
   VG_(message)(Vg_UserMsg, fmt, "I   refs:     ", Ir_total.a);
   VG_(message)(Vg_UserMsg, fmt, "I1  misses:   ", Ir_total.m1);
   VG_(message)(Vg_UserMsg, fmt, "L2i misses:   ", Ir_total.m2);

   p = 100;

   if (0 == Ir_total.a) Ir_total.a = 1;
   percentify(Ir_total.m1 * 100 * p / Ir_total.a, p, l1+1, buf1);
   VG_(message)(Vg_UserMsg, "I1  miss rate: %s", buf1);
                
   percentify(Ir_total.m2 * 100 * p / Ir_total.a, p, l1+1, buf1);
   VG_(message)(Vg_UserMsg, "L2i miss rate: %s", buf1);
   VG_(message)(Vg_UserMsg, "");

   /* D cache results.  Use the D_refs.rd and D_refs.wr values to determine the
    * width of columns 2 & 3. */
   D_total.a  = Dr_total.a  + Dw_total.a;
   D_total.m1 = Dr_total.m1 + Dw_total.m1;
   D_total.m2 = Dr_total.m2 + Dw_total.m2;
       
   /* Make format string, getting width right for numbers */
   VG_(sprintf)(fmt, "%%s %%,%dld  (%%,%dld rd + %%,%dld wr)", l1, l2, l3);

   VG_(message)(Vg_UserMsg, fmt, "D   refs:     ", 
                            D_total.a, Dr_total.a, Dw_total.a);
   VG_(message)(Vg_UserMsg, fmt, "D1  misses:   ",
                            D_total.m1, Dr_total.m1, Dw_total.m1);
   VG_(message)(Vg_UserMsg, fmt, "L2d misses:   ",
                            D_total.m2, Dr_total.m2, Dw_total.m2);

   p = 10;
   
   if (0 == D_total.a)   D_total.a = 1;
   if (0 == Dr_total.a) Dr_total.a = 1;
   if (0 == Dw_total.a) Dw_total.a = 1;
   percentify( D_total.m1 * 100 * p / D_total.a,  p, l1+1, buf1);
   percentify(Dr_total.m1 * 100 * p / Dr_total.a, p, l2+1, buf2);
   percentify(Dw_total.m1 * 100 * p / Dw_total.a, p, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "D1  miss rate: %s (%s   + %s  )", buf1, buf2,buf3);

   percentify( D_total.m2 * 100 * p / D_total.a,  p, l1+1, buf1);
   percentify(Dr_total.m2 * 100 * p / Dr_total.a, p, l2+1, buf2);
   percentify(Dw_total.m2 * 100 * p / Dw_total.a, p, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "L2d miss rate: %s (%s   + %s  )", buf1, buf2,buf3);
   VG_(message)(Vg_UserMsg, "");

   /* L2 overall results */

   L2_total   = Dr_total.m1 + Dw_total.m1 + Ir_total.m1;
   L2_total_r = Dr_total.m1 + Ir_total.m1;
   L2_total_w = Dw_total.m1;
   VG_(message)(Vg_UserMsg, fmt, "L2 refs:      ",
                            L2_total, L2_total_r, L2_total_w);

   L2_total_m  = Dr_total.m2 + Dw_total.m2 + Ir_total.m2;
   L2_total_mr = Dr_total.m2 + Ir_total.m2;
   L2_total_mw = Dw_total.m2;
   VG_(message)(Vg_UserMsg, fmt, "L2 misses:    ",
                            L2_total_m, L2_total_mr, L2_total_mw);

   percentify(L2_total_m  * 100 * p / (Ir_total.a + D_total.a),  p, l1+1, buf1);
   percentify(L2_total_mr * 100 * p / (Ir_total.a + Dr_total.a), p, l2+1, buf2);
   percentify(L2_total_mw * 100 * p / Dw_total.a, p, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "L2 miss rate:  %s (%s   + %s  )", buf1, buf2,buf3);
            

   // Various stats
   if (VG_(clo_verbosity) > 1) {
       int BB_lookups = full_debug_BBs      + fn_debug_BBs +
                        file_line_debug_BBs + no_debug_BBs;
      
       VG_(message)(Vg_DebugMsg, "");
       VG_(message)(Vg_DebugMsg, "Distinct files:   %d", distinct_files);
       VG_(message)(Vg_DebugMsg, "Distinct fns:     %d", distinct_fns);
       VG_(message)(Vg_DebugMsg, "Distinct lines:   %d", distinct_lines);
       VG_(message)(Vg_DebugMsg, "Distinct instrs:  %d", distinct_instrs);
       VG_(message)(Vg_DebugMsg, "BB lookups:       %d", BB_lookups);
       VG_(message)(Vg_DebugMsg, "With full      debug info:%3d%% (%d)", 
                    full_debug_BBs    * 100 / BB_lookups,
                    full_debug_BBs);
       VG_(message)(Vg_DebugMsg, "With file/line debug info:%3d%% (%d)", 
                    file_line_debug_BBs * 100 / BB_lookups,
                    file_line_debug_BBs);
       VG_(message)(Vg_DebugMsg, "With fn name   debug info:%3d%% (%d)", 
                    fn_debug_BBs * 100 / BB_lookups,
                    fn_debug_BBs);
       VG_(message)(Vg_DebugMsg, "With no        debug info:%3d%% (%d)", 
                    no_debug_BBs      * 100 / BB_lookups,
                    no_debug_BBs);
       VG_(message)(Vg_DebugMsg, "BBs Retranslated: %d", BB_retranslations);
   }
   VGP_POPCC(VgpCacheResults);
}

/*--------------------------------------------------------------------*/
/*--- Discarding BB info                                           ---*/
/*--------------------------------------------------------------------*/

// Called when a translation is invalidated due to code unloading.
static void cg_discard_basic_block_info ( Addr a, SizeT size )
{
   VgHashNode** prev_next_ptr;
   VgHashNode*  bbInfo;

   if (0) VG_(printf)( "discard_basic_block_info: %p, %llu\n", a, (ULong)size);

   // Get BB info, remove from table, free BB info.  Simple!
   bbInfo = VG_(HT_get_node)(instr_info_table, a, &prev_next_ptr);
   tl_assert(NULL != bbInfo);
   *prev_next_ptr = bbInfo->next;
   VG_(free)(bbInfo);
}

/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

static void parse_cache_opt ( cache_t* cache, char* opt )
{
   int   i = 0, i2, i3;

   // Option argument looks like "65536,2,64".
   // Find commas, replace with NULs to make three independent 
   // strings, then extract numbers, put NULs back.  Yuck.
   while (VG_(isdigit)(opt[i])) i++;
   if (',' == opt[i]) {
      opt[i++] = '\0';
      i2 = i;
   } else goto bad;
   while (VG_(isdigit)(opt[i])) i++;
   if (',' == opt[i]) {
      opt[i++] = '\0';
      i3 = i;
   } else goto bad;
   while (VG_(isdigit)(opt[i])) i++;
   if ('\0' != opt[i]) goto bad;

   cache->size      = (Int)VG_(atoll)(opt);
   cache->assoc     = (Int)VG_(atoll)(opt + i2);
   cache->line_size = (Int)VG_(atoll)(opt + i3);

   opt[i2-1] = ',';
   opt[i3-1] = ',';
   return;

  bad:
   VG_(bad_option)(opt);
}

static Bool cg_process_cmd_line_option(Char* arg)
{
   // 5 is length of "--I1="
   if      (VG_CLO_STREQN(5, arg, "--I1="))
      parse_cache_opt(&clo_I1_cache, &arg[5]);
   else if (VG_CLO_STREQN(5, arg, "--D1="))
      parse_cache_opt(&clo_D1_cache, &arg[5]);
   else if (VG_CLO_STREQN(5, arg, "--L2="))
      parse_cache_opt(&clo_L2_cache, &arg[5]);
   else
      return False;

   return True;
}

static void cg_print_usage(void)
{
   VG_(printf)(
"    --I1=<size>,<assoc>,<line_size>  set I1 cache manually\n"
"    --D1=<size>,<assoc>,<line_size>  set D1 cache manually\n"
"    --L2=<size>,<assoc>,<line_size>  set L2 cache manually\n"
   );
}

static void cg_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
   );
}

/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

static void cg_post_clo_init(void)
{
   cache_t I1c, D1c, L2c; 

   configure_caches(&I1c, &D1c, &L2c);

   cachesim_I1_initcache(I1c);
   cachesim_D1_initcache(D1c);
   cachesim_L2_initcache(L2c);

   VG_(register_profile_event)(VgpGetLineCC,     "get-lineCC");
   VG_(register_profile_event)(VgpCacheSimulate, "cache-simulate");
   VG_(register_profile_event)(VgpCacheResults,  "cache-results");
}

static void cg_pre_clo_init(void)
{
   Char* base_dir = NULL;

   VG_(details_name)            ("Cachegrind");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("an I1/D1/L2 cache profiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2005, and GNU GPL'd, by Nicholas Nethercote et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 155 );

   VG_(basic_tool_funcs)          (cg_post_clo_init,
                                   cg_instrument,
                                   cg_fini);

   VG_(needs_basic_block_discards)(cg_discard_basic_block_info);
   VG_(needs_command_line_options)(cg_process_cmd_line_option,
                                   cg_print_usage,
                                   cg_print_debug_usage);

   /* Get working directory */
   tl_assert( VG_(getcwd_alloc)(&base_dir) );

   /* Block is big enough for dir name + cachegrind.out.<pid> */
   cachegrind_out_file = VG_(malloc)((VG_(strlen)(base_dir) + 32)*sizeof(Char));
   VG_(sprintf)(cachegrind_out_file, "%s/cachegrind.out.%d",
                base_dir, VG_(getpid)());
   VG_(free)(base_dir);

   instr_info_table = VG_(HT_construct)();
}

VG_DETERMINE_INTERFACE_VERSION(cg_pre_clo_init, 0)

/*--------------------------------------------------------------------*/
/*--- end                                                cg_main.c ---*/
/*--------------------------------------------------------------------*/
