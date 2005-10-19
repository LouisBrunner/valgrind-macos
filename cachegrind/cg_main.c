
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
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_oset.h"
#include "pub_tool_profile.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_clientstate.h"

#include "cg_arch.h"
#include "cg_sim.c"

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

/* Set to 1 for very verbose debugging */
#define DEBUG_CG 0

#define MIN_LINE_SIZE         16
#define FILE_LEN              VKI_PATH_MAX
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
// - an ordered set of CCs.  CC indexing done by file/function/line (as
//   determined from the instrAddr).
// - Traversed for dumping stats at end in file/func/line hierarchy.

typedef struct {
   Char* file;
   Char* fn;
   Int   line;
}
CodeLoc;

typedef struct _LineCC LineCC;
struct _LineCC {
   CodeLoc loc;
   CC      Ir;
   CC      Dr;
   CC      Dw;
};

// First compare file, then fn, then line.
static Int cmp_CodeLoc_LineCC(void *vloc, void *vcc)
{
   Int res;
   CodeLoc* a = (CodeLoc*)vloc;
   CodeLoc* b = &(((LineCC*)vcc)->loc);

   res = VG_(strcmp)(a->file, b->file);
   if (0 != res)
      return res;

   res = VG_(strcmp)(a->fn, b->fn);
   if (0 != res)
      return res;

   return a->line - b->line;
}

static OSet* CC_table;

//------------------------------------------------------------
// Primary data structure #2: InstrInfo table
// - Holds the cached info about each instr that is used for simulation.
// - table(BB_start_addr, list(InstrInfo))
// - For each BB, each InstrInfo in the list holds info about the
//   instruction (instrLen, instrAddr, etc), plus a pointer to its line
//   CC.  This node is what's passed to the simulation function.
// - When BBs are discarded the relevant list(instr_details) is freed.

typedef struct _InstrInfo InstrInfo;
struct _InstrInfo {
   Addr    instr_addr;
   UChar   instr_len;
   LineCC* parent;         // parent line-CC
};

typedef struct _BB_info BB_info;
struct _BB_info {
   Addr      BB_addr;      // key;  MUST BE FIRST
   Int       n_instrs;
   InstrInfo instrs[0];
};

static OSet* instrInfoTable;

//------------------------------------------------------------
// Secondary data structure: string table
// - holds strings, avoiding dups
// - used for filenames and function names, each of which will be
//   pointed to by one or more CCs.
// - it also allows equality checks just by pointer comparison, which
//   is good when printing the output file at the end.

static OSet* stringTable;

//------------------------------------------------------------
// Stats
static Int  distinct_files      = 0;
static Int  distinct_fns        = 0;
static Int  distinct_lines      = 0;
static Int  distinct_instrs     = 0;

static Int  full_debugs         = 0;
static Int  file_line_debugs    = 0;
static Int  fn_debugs           = 0;
static Int  no_debugs           = 0;

/*------------------------------------------------------------*/
/*--- String table operations                              ---*/
/*------------------------------------------------------------*/

static Int stringCmp( void* key, void* elem )
{
   return VG_(strcmp)(*(Char**)key, *(Char**)elem);
}

// Get a permanent string;  either pull it out of the string table if it's
// been encountered before, or dup it and put it into the string table.
static Char* get_perm_string(Char* s)
{
   Char** s_ptr = VG_(OSet_Lookup)(stringTable, &s);
   if (s_ptr) {
      return *s_ptr;
   } else {
      Char** s_node = VG_(OSet_AllocNode)(stringTable, sizeof(Char*));
      *s_node = VG_(strdup)(s);
      VG_(OSet_Insert)(stringTable, s_node);
      return *s_node;
   }
}

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
      if (found_fn) full_debugs++;
      else          file_line_debugs++;
   } else {
      if (found_fn) fn_debugs++;
      else          no_debugs++;
   }
}

// Do a three step traversal: by file, then fn, then line.
// Returns a pointer to the line CC, creates a new one if necessary.
static LineCC* get_lineCC(Addr origAddr)
{
   Char    file[FILE_LEN], fn[FN_LEN];
   Int     line;
   CodeLoc loc;
   LineCC* lineCC;

   get_debug_info(origAddr, file, fn, &line);

   VGP_PUSHCC(VgpGetLineCC);

   loc.file = file;
   loc.fn   = fn;
   loc.line = line;

   lineCC = VG_(OSet_Lookup)(CC_table, &loc);
   if (!lineCC) {
      // Allocate and zero a new node.
      lineCC           = VG_(OSet_AllocNode)(CC_table, sizeof(LineCC));
      lineCC->loc.file = get_perm_string(loc.file);
      lineCC->loc.fn   = get_perm_string(loc.fn);
      lineCC->loc.line = loc.line;
      VG_(OSet_Insert)(CC_table, lineCC);
   }

   VGP_POPCC(VgpGetLineCC);
   return lineCC;
}

/*------------------------------------------------------------*/
/*--- Cache simulation functions                           ---*/
/*------------------------------------------------------------*/

static VG_REGPARM(1)
void log_1I_0D_cache_access(InstrInfo* n)
{
   //VG_(printf)("1I_0D :  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n",
   //             n, n->instr_addr, n->instr_len);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VG_REGPARM(2)
void log_2I_0D_cache_access(InstrInfo* n, InstrInfo* n2)
{
   //VG_(printf)("2I_0D : CC1addr=0x%010lx, i1addr=0x%010lx, i1size=%lu\n"
   //            "        CC2addr=0x%010lx, i2addr=0x%010lx, i2size=%lu\n",
   //            n,  n->instr_addr,  n->instr_len,
   //            n2, n2->instr_addr, n2->instr_len);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;
   cachesim_I1_doref(n2->instr_addr, n2->instr_len, 
                     &n2->parent->Ir.m1, &n2->parent->Ir.m2);
   n2->parent->Ir.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VG_REGPARM(3)
void log_3I_0D_cache_access(InstrInfo* n, InstrInfo* n2, InstrInfo* n3)
{
   //VG_(printf)("3I_0D : CC1addr=0x%010lx, i1addr=0x%010lx, i1size=%lu\n"
   //            "        CC2addr=0x%010lx, i2addr=0x%010lx, i2size=%lu\n"
   //            "        CC3addr=0x%010lx, i3addr=0x%010lx, i3size=%lu\n",
   //            n,  n->instr_addr,  n->instr_len,
   //            n2, n2->instr_addr, n2->instr_len,
   //            n3, n3->instr_addr, n3->instr_len);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;
   cachesim_I1_doref(n2->instr_addr, n2->instr_len, 
                     &n2->parent->Ir.m1, &n2->parent->Ir.m2);
   n2->parent->Ir.a++;
   cachesim_I1_doref(n3->instr_addr, n3->instr_len, 
                     &n3->parent->Ir.m1, &n3->parent->Ir.m2);
   n3->parent->Ir.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VG_REGPARM(3)
void log_1I_1Dr_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("1I_1Dr:  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n"
   //            "                               daddr=0x%010lx,  dsize=%lu\n",
   //            n, n->instr_addr, n->instr_len, data_addr, data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.m2);
   n->parent->Dr.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VG_REGPARM(3)
void log_1I_1Dw_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("1I_1Dw:  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n"
   //            "                               daddr=0x%010lx,  dsize=%lu\n",
   //            n, n->instr_addr, n->instr_len, data_addr, data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.m2);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.m2);
   n->parent->Dw.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VG_REGPARM(3)
void log_0I_1Dr_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("0I_1Dr:  CCaddr=0x%010lx,  daddr=0x%010lx,  dsize=%lu\n",
   //            n, data_addr, data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.m2);
   n->parent->Dr.a++;
   VGP_POPCC(VgpCacheSimulate);
}

static VG_REGPARM(3)
void log_0I_1Dw_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("0I_1Dw:  CCaddr=0x%010lx,  daddr=0x%010lx,  dsize=%lu\n",
   //            n, data_addr, data_size);
   VGP_PUSHCC(VgpCacheSimulate);
   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.m2);
   n->parent->Dw.a++;
   VGP_POPCC(VgpCacheSimulate);
}

/*------------------------------------------------------------*/
/*--- Instrumentation types and structures                 ---*/
/*------------------------------------------------------------*/

/* Maintain an ordered list of memory events which are outstanding, in
   the sense that no IR has yet been generated to do the relevant
   helper calls.  The BB is scanned top to bottom and memory events
   are added to the end of the list, merging with the most recent
   notified event where possible (Dw immediately following Dr and
   having the same size and EA can be merged).

   This merging is done so that for architectures which have
   load-op-store instructions (x86, amd64), the insn is treated as if
   it makes just one memory reference (a modify), rather than two (a
   read followed by a write at the same address).

   At various points the list will need to be flushed, that is, IR
   generated from it.  That must happen before any possible exit from
   the block (the end, or an IRStmt_Exit).  Flushing also takes place
   when there is no space to add a new event.

   If we require the simulation statistics to be up to date with
   respect to possible memory exceptions, then the list would have to
   be flushed before each memory reference.  That would however lose
   performance by inhibiting event-merging during flushing.

   Flushing the list consists of walking it start to end and emitting
   instrumentation IR for each event, in the order in which they
   appear.  It may be possible to emit a single call for two adjacent
   events in order to reduce the number of helper function calls made.
   For example, it could well be profitable to handle two adjacent Ir
   events with a single helper call.  */

typedef
   IRExpr 
   IRAtom;

typedef 
   enum { Event_Ir, Event_Dr, Event_Dw, Event_Dm }
   EventKind;

typedef
   struct {
      EventKind  ekind;       // All
      InstrInfo* inode;       // All;  inode for this event's instruction
      Int        datasize;    // Dr/Dw/Dm only
      IRAtom*    dataEA;      // Dr/Dw/Dm only;  IR ATOM ONLY
   }
   Event;

/* Up to this many unnotified events are allowed.  Number is
   arbitrary.  Larger numbers allow more event merging to occur, but
   potentially induce more spilling due to extending live ranges of
   address temporaries. */
#define N_EVENTS 16


/* A struct which holds all the running state during instrumentation.
   Mostly to avoid passing loads of parameters everywhere. */
typedef
   struct {
      /* The current outstanding-memory-event list. */
      Event events[N_EVENTS];
      Int   events_used;

      /* The array of InstrInfo bins for the BB. */
      BB_info* bbInfo;

      /* Number InstrInfo bins 'used' so far. */
      Int bbInfo_i;

      /* The output BB being constructed. */
      IRBB* bbOut;
   }
   CgState;


/*------------------------------------------------------------*/
/*--- Instrumentation main                                 ---*/
/*------------------------------------------------------------*/

// Note that origAddr is the real origAddr, not the address of the first
// instruction in the block (they can be different due to redirection).
static
BB_info* get_BB_info(IRBB* bbIn, Addr origAddr)
{
   Int      i, n_instrs;
   IRStmt*  st;
   BB_info* bbInfo;

   // Count number of original instrs in BB
   n_instrs = 0;
   for (i = 0; i < bbIn->stmts_used; i++) {
      st = bbIn->stmts[i];
      if (Ist_IMark == st->tag) n_instrs++;
   }

   // Check that we don't have an entry for this BB in the instr-info table.
   // If this assertion fails, there has been some screwup:  some
   // translations must have been discarded but Cachegrind hasn't discarded
   // the corresponding entries in the instr-info table.
   bbInfo = VG_(OSet_Lookup)(instrInfoTable, &origAddr);
   tl_assert(NULL == bbInfo);

   // BB never translated before (at this address, at least;  could have
   // been unloaded and then reloaded elsewhere in memory)
   bbInfo = VG_(OSet_AllocNode)(instrInfoTable,
                                sizeof(BB_info) + n_instrs*sizeof(InstrInfo)); 
   bbInfo->BB_addr  = origAddr;
   bbInfo->n_instrs = n_instrs;
   VG_(OSet_Insert)( instrInfoTable, bbInfo );
   distinct_instrs++;

   return bbInfo;
}


static void showEvent ( Event* ev )
{
   switch (ev->ekind) {
      case Event_Ir: 
         VG_(printf)("Ir %p\n", ev->inode);
         break;
      case Event_Dr:
         VG_(printf)("Dr %p %d EA=", ev->inode, ev->datasize);
         ppIRExpr(ev->dataEA); 
         VG_(printf)("\n");
         break;
      case Event_Dw:
         VG_(printf)("Dw %p %d EA=", ev->inode, ev->datasize);
         ppIRExpr(ev->dataEA); 
         VG_(printf)("\n");
         break;
      case Event_Dm:
         VG_(printf)("Dm %p %d EA=", ev->inode, ev->datasize);
         ppIRExpr(ev->dataEA); 
         VG_(printf)("\n");
         break;
      default: 
         tl_assert(0);
         break;
   }
}

// Reserve and initialise an InstrInfo for the first mention of a new insn.
static
InstrInfo* setup_InstrInfo ( CgState* cgs, Addr instr_addr, UInt instr_len )
{
   InstrInfo* i_node;
   tl_assert(cgs->bbInfo_i >= 0);
   tl_assert(cgs->bbInfo_i < cgs->bbInfo->n_instrs);
   i_node = &cgs->bbInfo->instrs[ cgs->bbInfo_i ];
   i_node->instr_addr = instr_addr;
   i_node->instr_len  = instr_len;
   i_node->parent     = get_lineCC(instr_addr);
   cgs->bbInfo_i++;
   return i_node;
}


/* Generate code for all outstanding memory events, and mark the queue
   empty.  Code is generated into cgs->bbOut, and this activity
   'consumes' slots in cgs->bbInfo. */

static void flushEvents ( CgState* cgs )
{
   Int        i, regparms;
   Char*      helperName;
   void*      helperAddr;
   IRExpr**   argv;
   IRExpr*    i_node_expr;
   IRDirty*   di;
   Event*     ev;
   Event*     ev2;
   Event*     ev3;

   i = 0;
   while (i < cgs->events_used) {

      helperName = NULL;
      helperAddr = NULL;
      argv       = NULL;
      regparms   = 0;

      /* generate IR to notify event i and possibly the ones
         immediately following it. */
      tl_assert(i >= 0 && i < cgs->events_used);

      ev  = &cgs->events[i];
      ev2 = ( i < cgs->events_used-1 ? &cgs->events[i+1] : NULL );
      ev3 = ( i < cgs->events_used-2 ? &cgs->events[i+2] : NULL );
      
      if (DEBUG_CG) {
         VG_(printf)("   flush "); 
         showEvent( ev );
      }

      i_node_expr = mkIRExpr_HWord( (HWord)ev->inode );

      /* Decide on helper fn to call and args to pass it, and advance
         i appropriately. */
      switch (ev->ekind) {
         case Event_Ir:
            /* Merge with a following Dr/Dm if it is from this insn. */
            if (ev2 && (ev2->ekind == Event_Dr || ev2->ekind == Event_Dm)) {
               tl_assert(ev2->inode == ev->inode);
               helperName = "log_1I_1Dr_cache_access";
               helperAddr = &log_1I_1Dr_cache_access;
               argv = mkIRExprVec_3( i_node_expr,
                                     ev2->dataEA,
                                     mkIRExpr_HWord( ev2->datasize ) );
               regparms = 3;
               i += 2;
            }
            /* Merge with a following Dw if it is from this insn. */
            else
            if (ev2 && ev2->ekind == Event_Dw) {
               tl_assert(ev2->inode == ev->inode);
               helperName = "log_1I_1Dw_cache_access";
               helperAddr = &log_1I_1Dw_cache_access;
               argv = mkIRExprVec_3( i_node_expr,
                                     ev2->dataEA,
                                     mkIRExpr_HWord( ev2->datasize ) );
               regparms = 3;
               i += 2;
            }
            /* Merge with two following Irs if possible. */
            else
            if (ev2 && ev3 && ev2->ekind == Event_Ir && ev3->ekind == Event_Ir)
            {
               helperName = "log_3I_0D_cache_access";
               helperAddr = &log_3I_0D_cache_access;
               argv = mkIRExprVec_3( i_node_expr, 
                                     mkIRExpr_HWord( (HWord)ev2->inode ), 
                                     mkIRExpr_HWord( (HWord)ev3->inode ) );
               regparms = 3;
               i += 3;
            }
            /* Merge with a following Ir if possible. */
            else
            if (ev2 && ev2->ekind == Event_Ir) {
               helperName = "log_2I_0D_cache_access";
               helperAddr = &log_2I_0D_cache_access;
               argv = mkIRExprVec_2( i_node_expr,
                                     mkIRExpr_HWord( (HWord)ev2->inode ) );
               regparms = 2;
               i += 2;
            }
            /* No merging possible; emit as-is. */
            else {
               // Assertion: this Event_Ir must be the last one in the
               // events buffer, otherwise it would have been merged with a
               // following event.
               tl_assert(!ev2 && !ev3);
               helperName = "log_1I_0D_cache_access";
               helperAddr = &log_1I_0D_cache_access;
               argv = mkIRExprVec_1( i_node_expr );
               regparms = 1;
               i++;
            }
            break;
         case Event_Dr:
         case Event_Dm:
            helperName = "log_0I_1Dr_cache_access";
            helperAddr = &log_0I_1Dr_cache_access;
            argv = mkIRExprVec_3( i_node_expr, 
                                  ev->dataEA, 
                                  mkIRExpr_HWord( ev->datasize ) );
            regparms = 3;
            i++;
            break;
         case Event_Dw:
            helperName = "log_0I_1Dw_cache_access";
            helperAddr = &log_0I_1Dw_cache_access;
            argv = mkIRExprVec_3( i_node_expr,
                                  ev->dataEA, 
                                  mkIRExpr_HWord( ev->datasize ) );
            regparms = 3;
            i++;
            break;
         default:
            tl_assert(0);
      }

      /* Add the helper. */
      tl_assert(helperName);
      tl_assert(helperAddr);
      tl_assert(argv);
      di = unsafeIRDirty_0_N( regparms, helperName, helperAddr, argv);
      addStmtToIRBB( cgs->bbOut, IRStmt_Dirty(di) );
   }

   cgs->events_used = 0;
}

static void addEvent_Ir ( CgState* cgs, InstrInfo* inode )
{
   Event* evt;
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   evt->ekind    = Event_Ir;
   evt->inode    = inode;
   evt->datasize = 0;
   evt->dataEA   = NULL; /*paranoia*/
   cgs->events_used++;
}

static
void addEvent_Dr ( CgState* cgs, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* evt;
   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= MIN_LINE_SIZE);
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   evt->ekind    = Event_Dr;
   evt->inode    = inode;
   evt->datasize = datasize;
   evt->dataEA   = ea;
   cgs->events_used++;
}

static
void addEvent_Dw ( CgState* cgs, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* lastEvt;
   Event* evt;

   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= MIN_LINE_SIZE);

   /* Is it possible to merge this write with the preceding read? */
   lastEvt = &cgs->events[cgs->events_used-1];
   if (cgs->events_used > 0
    && lastEvt->ekind    == Event_Dr
    && lastEvt->datasize == datasize
    && lastEvt->inode    == inode
    && eqIRAtom(lastEvt->dataEA, ea))
   {
      lastEvt->ekind = Event_Dm;
      return;
   }

   /* No.  Add as normal. */
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   evt->ekind    = Event_Dw;
   evt->inode    = inode;
   evt->datasize = datasize;
   evt->dataEA   = ea;
   cgs->events_used++;
}

////////////////////////////////////////////////////////////


static
IRBB* cg_instrument ( IRBB* bbIn, VexGuestLayout* layout, 
                      Addr64 orig_addr_noredir, VexGuestExtents* vge,
                      IRType gWordTy, IRType hWordTy )
{
   Int        i, isize;
   IRStmt*    st;
   Addr64     cia; /* address of current insn */
   CgState    cgs;
   IRTypeEnv* tyenv = bbIn->tyenv;
   InstrInfo* curr_inode = NULL;


   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Set up BB, including copying of the where-next stuff. */
   cgs.bbOut           = emptyIRBB();
   cgs.bbOut->tyenv    = dopyIRTypeEnv(tyenv);
   tl_assert( isIRAtom(bbIn->next) );
   cgs.bbOut->next     = dopyIRExpr(bbIn->next);
   cgs.bbOut->jumpkind = bbIn->jumpkind;

   // Get the first statement, and initial cia from it
   i = 0;
   tl_assert(bbIn->stmts_used > 0);
   st = bbIn->stmts[0];
   tl_assert(Ist_IMark == st->tag);
   cia = st->Ist.IMark.addr;

   // Set up running state and get block info
   cgs.events_used = 0;
   cgs.bbInfo      = get_BB_info(bbIn, (Addr)orig_addr_noredir);
   cgs.bbInfo_i    = 0;

   if (DEBUG_CG)
      VG_(printf)("\n\n---------- cg_instrument ----------\n");

   // Traverse the block, initialising inodes, adding events and flushing as
   // necessary.
   for (i = 0; i < bbIn->stmts_used; i++) {

      st = bbIn->stmts[i];
      tl_assert(isFlatIRStmt(st));

      switch (st->tag) {
         case Ist_NoOp:
         case Ist_AbiHint:
         case Ist_Put:
         case Ist_PutI:
         case Ist_MFence:
            break;

         case Ist_IMark:
            cia   = st->Ist.IMark.addr;
            isize = st->Ist.IMark.len;

            // If Vex fails to decode an instruction, the size will be zero.
            // Pretend otherwise.
            if (isize == 0) isize = VG_MIN_INSTR_SZB;

            // Check size.  XXX: broken for client requests!
            tl_assert(VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB);

            // Get space for and init the inode, record it as the current one.
            // Subsequent Dr/Dw/Dm events from the same instruction will 
            // also use it.
            curr_inode = setup_InstrInfo(&cgs, cia, isize);

            addEvent_Ir( &cgs, curr_inode );
            break;

         case Ist_Tmp: {
            IRExpr* data = st->Ist.Tmp.data;
            if (data->tag == Iex_Load) {
               IRExpr* aexpr = data->Iex.Load.addr;
               // Note also, endianness info is ignored.  I guess
               // that's not interesting.
               addEvent_Dr( &cgs, curr_inode, sizeofIRType(data->Iex.Load.ty), 
                                  aexpr );
            }
            break;
         }

         case Ist_Store: {
            IRExpr* data  = st->Ist.Store.data;
            IRExpr* aexpr = st->Ist.Store.addr;
            addEvent_Dw( &cgs, curr_inode, 
                         sizeofIRType(typeOfIRExpr(tyenv, data)), aexpr );
            break;
         }

         case Ist_Dirty: {
            Int      dataSize;
            IRDirty* d = st->Ist.Dirty.details;
            if (d->mFx != Ifx_None) {
               /* This dirty helper accesses memory.  Collect the details. */
               tl_assert(d->mAddr != NULL);
               tl_assert(d->mSize != 0);
               dataSize = d->mSize;
               // Large (eg. 28B, 108B, 512B on x86) data-sized
               // instructions will be done inaccurately, but they're
               // very rare and this avoids errors from hitting more
               // than two cache lines in the simulation.
               if (dataSize > MIN_LINE_SIZE)
                  dataSize = MIN_LINE_SIZE;
               if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
                  addEvent_Dr( &cgs, curr_inode, dataSize, d->mAddr );
               if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
                  addEvent_Dw( &cgs, curr_inode, dataSize, d->mAddr );
            } else {
               tl_assert(d->mAddr == NULL);
               tl_assert(d->mSize == 0);
            }
            break;
         }

         case Ist_Exit:
            /* We may never reach the next statement, so need to flush
               all outstanding transactions now. */
            flushEvents( &cgs );
            break;

         default:
            tl_assert(0);
            break;
      }

      /* Copy the original statement */
      addStmtToIRBB( cgs.bbOut, st );

      if (DEBUG_CG) {
         ppIRStmt(st);
         VG_(printf)("\n");
      }
   }

   /* At the end of the bb.  Flush outstandings. */
   flushEvents( &cgs );

   /* done.  stay sane ... */
   tl_assert(cgs.bbInfo_i == cgs.bbInfo->n_instrs);

   if (DEBUG_CG) {
      VG_(printf)( "goto {");
      ppIRJumpKind(bbIn->jumpkind);
      VG_(printf)( "} ");
      ppIRExpr( bbIn->next );
      VG_(printf)( "}\n");
   }

   return cgs.bbOut;
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
   VG_(configure_caches)( I1c, D1c, L2c, (3 == n_clos) );

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

static void fprint_CC_table_and_calc_totals(void)
{
   Int     i, fd;
   SysRes  sres;
   Char    buf[512], *currFile = NULL, *currFn = NULL;
   LineCC* lineCC;

   VGP_PUSHCC(VgpCacheResults);

   sres = VG_(open)(cachegrind_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                                         VKI_S_IRUSR|VKI_S_IWUSR);
   if (sres.isError) {
      // If the file can't be opened for whatever reason (conflict
      // between multiple cachegrinded processes?), give up now.
      VG_(message)(Vg_UserMsg,
         "error: can't open cache simulation output file '%s'",
         cachegrind_out_file );
      VG_(message)(Vg_UserMsg,
         "       ... so simulation results will be missing.");
      return;
   } else {
      fd = sres.val;
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
   if (VG_(args_the_exename)) {
      VG_(write)(fd, " ", 1);
      VG_(write)(fd, VG_(args_the_exename), 
                     VG_(strlen)( VG_(args_the_exename) ));
   }
   for (i = 0; i < VG_(args_for_client).used; i++) {
      if (VG_(args_for_client).strs[i]) {
         VG_(write)(fd, " ", 1);
         VG_(write)(fd, VG_(args_for_client).strs[i], 
                        VG_(strlen)(VG_(args_for_client).strs[i]));
      }
   }
   // "events:" line
   VG_(sprintf)(buf, "\nevents: Ir I1mr I2mr Dr D1mr D2mr Dw D1mw D2mw\n");
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   // Traverse every lineCC
   VG_(OSet_ResetIter)(CC_table);
   while ( (lineCC = VG_(OSet_Next)(CC_table)) ) {
      // If we've hit a new file, print a "fl=" line.  Note that because
      // each string is stored exactly once in the string table, we can use
      // pointer comparison rather than strcmp() to test for equality, which
      // is good because most of the time the comparisons are equal and so
      // the whole strings would have to be traversed.
      if ( lineCC->loc.file != currFile ) {
         currFile = lineCC->loc.file;
         VG_(sprintf)(buf, "fl=%s\n", currFile);
         VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
         distinct_files++;
      }
      // If we've hit a new function, print a "fn=" line.
      if ( lineCC->loc.fn != currFn ) {
         currFn = lineCC->loc.fn;
         VG_(sprintf)(buf, "fn=%s\n", currFn);
         VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
         distinct_fns++;
      }

      // Print the LineCC
      VG_(sprintf)(buf, "%u %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
                         lineCC->loc.line,
                         lineCC->Ir.a, lineCC->Ir.m1, lineCC->Ir.m2, 
                         lineCC->Dr.a, lineCC->Dr.m1, lineCC->Dr.m2,
                         lineCC->Dw.a, lineCC->Dw.m1, lineCC->Dw.m2);
      VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

      // Update summary stats
      Ir_total.a  += lineCC->Ir.a;
      Ir_total.m1 += lineCC->Ir.m1;
      Ir_total.m2 += lineCC->Ir.m2;
      Dr_total.a  += lineCC->Dr.a;
      Dr_total.m1 += lineCC->Dr.m1;
      Dr_total.m2 += lineCC->Dr.m2;
      Dw_total.a  += lineCC->Dw.a;
      Dw_total.m1 += lineCC->Dw.m1;
      Dw_total.m2 += lineCC->Dw.m2;

      distinct_lines++;
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

static void cg_fini(Int exitcode)
{
   static Char buf1[128], buf2[128], buf3[128], fmt [128];

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
   VG_(sprintf)(fmt, "%%s %%,%dllu", l1);

   VG_(message)(Vg_UserMsg, fmt, "I   refs:     ", Ir_total.a);
   VG_(message)(Vg_UserMsg, fmt, "I1  misses:   ", Ir_total.m1);
   VG_(message)(Vg_UserMsg, fmt, "L2i misses:   ", Ir_total.m2);

   p = 100;

   if (0 == Ir_total.a) Ir_total.a = 1;
   VG_(percentify)(Ir_total.m1, Ir_total.a, 2, l1+1, buf1);
   VG_(message)(Vg_UserMsg, "I1  miss rate: %s", buf1);

   VG_(percentify)(Ir_total.m2, Ir_total.a, 2, l1+1, buf1);
   VG_(message)(Vg_UserMsg, "L2i miss rate: %s", buf1);
   VG_(message)(Vg_UserMsg, "");

   /* D cache results.  Use the D_refs.rd and D_refs.wr values to determine the
    * width of columns 2 & 3. */
   D_total.a  = Dr_total.a  + Dw_total.a;
   D_total.m1 = Dr_total.m1 + Dw_total.m1;
   D_total.m2 = Dr_total.m2 + Dw_total.m2;

   /* Make format string, getting width right for numbers */
   VG_(sprintf)(fmt, "%%s %%,%dllu  (%%,%dllu rd + %%,%dllu wr)", l1, l2, l3);

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
   VG_(percentify)( D_total.m1,  D_total.a, 1, l1+1, buf1);
   VG_(percentify)(Dr_total.m1, Dr_total.a, 1, l2+1, buf2);
   VG_(percentify)(Dw_total.m1, Dw_total.a, 1, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "D1  miss rate: %s (%s   + %s  )", buf1, buf2,buf3);

   VG_(percentify)( D_total.m2,  D_total.a, 1, l1+1, buf1);
   VG_(percentify)(Dr_total.m2, Dr_total.a, 1, l2+1, buf2);
   VG_(percentify)(Dw_total.m2, Dw_total.a, 1, l3+1, buf3);
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

   VG_(percentify)(L2_total_m,  (Ir_total.a + D_total.a),  1, l1+1, buf1);
   VG_(percentify)(L2_total_mr, (Ir_total.a + Dr_total.a), 1, l2+1, buf2);
   VG_(percentify)(L2_total_mw, Dw_total.a,                1, l3+1, buf3);
   VG_(message)(Vg_UserMsg, "L2 miss rate:  %s (%s   + %s  )", buf1, buf2,buf3);


   // Various stats
   if (VG_(clo_verbosity) > 1) {
       Int debug_lookups = full_debugs      + fn_debugs +
                           file_line_debugs + no_debugs;

       VG_(message)(Vg_DebugMsg, "");
       VG_(message)(Vg_DebugMsg, "cachegrind: distinct files: %d", distinct_files);
       VG_(message)(Vg_DebugMsg, "cachegrind: distinct fns:   %d", distinct_fns);
       VG_(message)(Vg_DebugMsg, "cachegrind: distinct lines: %d", distinct_lines);
       VG_(message)(Vg_DebugMsg, "cachegrind: distinct instrs:%d", distinct_instrs);
       VG_(message)(Vg_DebugMsg, "cachegrind: debug lookups      : %d", debug_lookups);
       VG_(message)(Vg_DebugMsg, "cachegrind: with full      info:%3d%% (%d)", 
                    full_debugs * 100 / debug_lookups, full_debugs);
       VG_(message)(Vg_DebugMsg, "cachegrind: with file/line info:%3d%% (%d)", 
                    file_line_debugs * 100 / debug_lookups, file_line_debugs);
       VG_(message)(Vg_DebugMsg, "cachegrind: with fn name   info:%3d%% (%d)", 
                    fn_debugs * 100 / debug_lookups, fn_debugs);
       VG_(message)(Vg_DebugMsg, "cachegrind: with zero      info:%3d%% (%d)", 
                    no_debugs * 100 / debug_lookups, no_debugs);
       VG_(message)(Vg_DebugMsg, "cachegrind: string table size: %u",
                    VG_(OSet_Size)(stringTable));
       VG_(message)(Vg_DebugMsg, "cachegrind: CC table size: %u",
                    VG_(OSet_Size)(CC_table));
       VG_(message)(Vg_DebugMsg, "cachegrind: InstrInfo table size: %u",
                    VG_(OSet_Size)(instrInfoTable));
   }
   VGP_POPCC(VgpCacheResults);
}

/*--------------------------------------------------------------------*/
/*--- Discarding BB info                                           ---*/
/*--------------------------------------------------------------------*/

// Called when a translation is removed from the translation cache for
// any reason at all: to free up space, because the guest code was
// unmapped or modified, or for any arbitrary reason.
static
void cg_discard_basic_block_info ( Addr64 orig_addr64, VexGuestExtents vge )
{
   BB_info* bbInfo;
   Addr     orig_addr = (Addr)orig_addr64;

   tl_assert(vge.n_used > 0);

   if (DEBUG_CG)
      VG_(printf)( "discard_basic_block_info: %p, %p, %llu\n", 
                   (void*)(Addr)orig_addr,
                   (void*)(Addr)vge.base[0], (ULong)vge.len[0]);

   // Get BB info, remove from table, free BB info.  Simple!  Note that we
   // use orig_addr, not the first instruction address in vge.
   bbInfo = VG_(OSet_Remove)(instrInfoTable, &orig_addr);
   tl_assert(NULL != bbInfo);
   VG_(OSet_FreeNode)(instrInfoTable, bbInfo);
}

/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

static void parse_cache_opt ( cache_t* cache, Char* opt )
{
   Int i = 0, i2, i3;

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

static Char base_dir[VKI_PATH_MAX];

static void cg_pre_clo_init(void)
{
   VG_(details_name)            ("Cachegrind");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("an I1/D1/L2 cache profiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2005, and GNU GPL'd, by Nicholas Nethercote et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 245 );

   VG_(basic_tool_funcs)          (cg_post_clo_init,
                                   cg_instrument,
                                   cg_fini);

   VG_(needs_basic_block_discards)(cg_discard_basic_block_info);
   VG_(needs_command_line_options)(cg_process_cmd_line_option,
                                   cg_print_usage,
                                   cg_print_debug_usage);

   /* Get working directory */
   tl_assert( VG_(getcwd)(base_dir, VKI_PATH_MAX) );

   /* Block is big enough for dir name + cachegrind.out.<pid> */
   cachegrind_out_file = VG_(malloc)((VG_(strlen)(base_dir) + 32)*sizeof(Char));
   VG_(sprintf)(cachegrind_out_file, "%s/cachegrind.out.%d",
                base_dir, VG_(getpid)());

   CC_table = VG_(OSet_Create)(offsetof(LineCC, loc),
                               cmp_CodeLoc_LineCC,
                               VG_(malloc), VG_(free));
   instrInfoTable = VG_(OSet_Create)(/*keyOff*/0,
                                     NULL,
                                     VG_(malloc), VG_(free));
   stringTable    = VG_(OSet_Create)(/*keyOff*/0,
                                     stringCmp,
                                     VG_(malloc), VG_(free));
}

VG_DETERMINE_INTERFACE_VERSION(cg_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

