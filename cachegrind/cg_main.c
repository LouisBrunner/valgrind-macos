
/*--------------------------------------------------------------------*/
/*--- Cachegrind: everything but the simulation itself.            ---*/
/*---                                                    cg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2011 Nicholas Nethercote
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
#include "pub_tool_vki.h"
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
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_machine.h"      // VG_(fnptr_to_fnentry)

#include "cg_arch.h"
#include "cg_sim.c"
#include "cg_branchpred.c"

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

/* Set to 1 for very verbose debugging */
#define DEBUG_CG 0

#define MIN_LINE_SIZE         16
#define FILE_LEN              VKI_PATH_MAX
#define FN_LEN                256

/*------------------------------------------------------------*/
/*--- Options                                              ---*/
/*------------------------------------------------------------*/

static Bool  clo_cache_sim  = True;  /* do cache simulation? */
static Bool  clo_branch_sim = False; /* do branch simulation? */
static Char* clo_cachegrind_out_file = "cachegrind.out.%p";

/*------------------------------------------------------------*/
/*--- Types and Data Structures                            ---*/
/*------------------------------------------------------------*/

typedef
   struct {
      ULong a;  /* total # memory accesses of this kind */
      ULong m1; /* misses in the first level cache */
      ULong mL; /* misses in the second level cache */
   }
   CacheCC;

typedef
   struct {
      ULong b;  /* total # branches of this kind */
      ULong mp; /* number of branches mispredicted */
   }
   BranchCC;

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

typedef struct {
   CodeLoc  loc; /* Source location that these counts pertain to */
   CacheCC  Ir;  /* Insn read counts */
   CacheCC  Dr;  /* Data read counts */
   CacheCC  Dw;  /* Data write/modify counts */
   BranchCC Bc;  /* Conditional branch counts */
   BranchCC Bi;  /* Indirect branch counts */
} LineCC;

// First compare file, then fn, then line.
static Word cmp_CodeLoc_LineCC(const void *vloc, const void *vcc)
{
   Word res;
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
// - table(SB_start_addr, list(InstrInfo))
// - For each SB, each InstrInfo in the list holds info about the
//   instruction (instrLen, instrAddr, etc), plus a pointer to its line
//   CC.  This node is what's passed to the simulation function.
// - When SBs are discarded the relevant list(instr_details) is freed.

typedef struct _InstrInfo InstrInfo;
struct _InstrInfo {
   Addr    instr_addr;
   UChar   instr_len;
   LineCC* parent;         // parent line-CC
};

typedef struct _SB_info SB_info;
struct _SB_info {
   Addr      SB_addr;      // key;  MUST BE FIRST
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

static Word stringCmp( const void* key, const void* elem )
{
   return VG_(strcmp)(*(Char**)key, *(Char**)elem);
}

// Get a permanent string;  either pull it out of the string table if it's
// been encountered before, or dup it and put it into the string table.
static Char* get_perm_string(Char* s)
{
   Char** s_ptr = VG_(OSetGen_Lookup)(stringTable, &s);
   if (s_ptr) {
      return *s_ptr;
   } else {
      Char** s_node = VG_(OSetGen_AllocNode)(stringTable, sizeof(Char*));
      *s_node = VG_(strdup)("cg.main.gps.1", s);
      VG_(OSetGen_Insert)(stringTable, s_node);
      return *s_node;
   }
}

/*------------------------------------------------------------*/
/*--- CC table operations                                  ---*/
/*------------------------------------------------------------*/

static void get_debug_info(Addr instr_addr, Char file[FILE_LEN],
                           Char fn[FN_LEN], Int* line)
{
   Char dir[FILE_LEN];
   Bool found_dirname;
   Bool found_file_line = VG_(get_filename_linenum)(
                             instr_addr, 
                             file, FILE_LEN,
                             dir,  FILE_LEN, &found_dirname,
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

   if (found_dirname) {
      // +1 for the '/'.
      tl_assert(VG_(strlen)(dir) + VG_(strlen)(file) + 1 < FILE_LEN);
      VG_(strcat)(dir, "/");     // Append '/'
      VG_(strcat)(dir, file);    // Append file to dir
      VG_(strcpy)(file, dir);    // Move dir+file to file
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

   loc.file = file;
   loc.fn   = fn;
   loc.line = line;

   lineCC = VG_(OSetGen_Lookup)(CC_table, &loc);
   if (!lineCC) {
      // Allocate and zero a new node.
      lineCC           = VG_(OSetGen_AllocNode)(CC_table, sizeof(LineCC));
      lineCC->loc.file = get_perm_string(loc.file);
      lineCC->loc.fn   = get_perm_string(loc.fn);
      lineCC->loc.line = loc.line;
      lineCC->Ir.a     = 0;
      lineCC->Ir.m1    = 0;
      lineCC->Ir.mL    = 0;
      lineCC->Dr.a     = 0;
      lineCC->Dr.m1    = 0;
      lineCC->Dr.mL    = 0;
      lineCC->Dw.a     = 0;
      lineCC->Dw.m1    = 0;
      lineCC->Dw.mL    = 0;
      lineCC->Bc.b     = 0;
      lineCC->Bc.mp    = 0;
      lineCC->Bi.b     = 0;
      lineCC->Bi.mp    = 0;
      VG_(OSetGen_Insert)(CC_table, lineCC);
   }

   return lineCC;
}

/*------------------------------------------------------------*/
/*--- Cache simulation functions                           ---*/
/*------------------------------------------------------------*/

// Only used with --cache-sim=no.
static VG_REGPARM(1)
void log_1I(InstrInfo* n)
{
   n->parent->Ir.a++;
}

// Only used with --cache-sim=no.
static VG_REGPARM(2)
void log_2I(InstrInfo* n, InstrInfo* n2)
{
   n->parent->Ir.a++;
   n2->parent->Ir.a++;
}

// Only used with --cache-sim=no.
static VG_REGPARM(3)
void log_3I(InstrInfo* n, InstrInfo* n2, InstrInfo* n3)
{
   n->parent->Ir.a++;
   n2->parent->Ir.a++;
   n3->parent->Ir.a++;
}

static VG_REGPARM(1)
void log_1I_0D_cache_access(InstrInfo* n)
{
   //VG_(printf)("1I_0D :  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n",
   //             n, n->instr_addr, n->instr_len);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
}

static VG_REGPARM(2)
void log_2I_0D_cache_access(InstrInfo* n, InstrInfo* n2)
{
   //VG_(printf)("2I_0D : CC1addr=0x%010lx, i1addr=0x%010lx, i1size=%lu\n"
   //            "        CC2addr=0x%010lx, i2addr=0x%010lx, i2size=%lu\n",
   //            n,  n->instr_addr,  n->instr_len,
   //            n2, n2->instr_addr, n2->instr_len);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
   cachesim_I1_doref(n2->instr_addr, n2->instr_len, 
                     &n2->parent->Ir.m1, &n2->parent->Ir.mL);
   n2->parent->Ir.a++;
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
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;
   cachesim_I1_doref(n2->instr_addr, n2->instr_len, 
                     &n2->parent->Ir.m1, &n2->parent->Ir.mL);
   n2->parent->Ir.a++;
   cachesim_I1_doref(n3->instr_addr, n3->instr_len, 
                     &n3->parent->Ir.m1, &n3->parent->Ir.mL);
   n3->parent->Ir.a++;
}

static VG_REGPARM(3)
void log_1I_1Dr_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("1I_1Dr:  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n"
   //            "                               daddr=0x%010lx,  dsize=%lu\n",
   //            n, n->instr_addr, n->instr_len, data_addr, data_size);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.mL);
   n->parent->Dr.a++;
}

static VG_REGPARM(3)
void log_1I_1Dw_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("1I_1Dw:  CCaddr=0x%010lx,  iaddr=0x%010lx,  isize=%lu\n"
   //            "                               daddr=0x%010lx,  dsize=%lu\n",
   //            n, n->instr_addr, n->instr_len, data_addr, data_size);
   cachesim_I1_doref(n->instr_addr, n->instr_len, 
                     &n->parent->Ir.m1, &n->parent->Ir.mL);
   n->parent->Ir.a++;

   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.mL);
   n->parent->Dw.a++;
}

static VG_REGPARM(3)
void log_0I_1Dr_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("0I_1Dr:  CCaddr=0x%010lx,  daddr=0x%010lx,  dsize=%lu\n",
   //            n, data_addr, data_size);
   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dr.m1, &n->parent->Dr.mL);
   n->parent->Dr.a++;
}

static VG_REGPARM(3)
void log_0I_1Dw_cache_access(InstrInfo* n, Addr data_addr, Word data_size)
{
   //VG_(printf)("0I_1Dw:  CCaddr=0x%010lx,  daddr=0x%010lx,  dsize=%lu\n",
   //            n, data_addr, data_size);
   cachesim_D1_doref(data_addr, data_size, 
                     &n->parent->Dw.m1, &n->parent->Dw.mL);
   n->parent->Dw.a++;
}

/* For branches, we consult two different predictors, one which
   predicts taken/untaken for conditional branches, and the other
   which predicts the branch target address for indirect branches
   (jump-to-register style ones). */

static VG_REGPARM(2)
void log_cond_branch(InstrInfo* n, Word taken)
{
   //VG_(printf)("cbrnch:  CCaddr=0x%010lx,  taken=0x%010lx\n",
   //             n, taken);
   n->parent->Bc.b++;
   n->parent->Bc.mp 
      += (1 & do_cond_branch_predict(n->instr_addr, taken));
}

static VG_REGPARM(2)
void log_ind_branch(InstrInfo* n, UWord actual_dst)
{
   //VG_(printf)("ibrnch:  CCaddr=0x%010lx,    dst=0x%010lx\n",
   //             n, actual_dst);
   n->parent->Bi.b++;
   n->parent->Bi.mp
      += (1 & do_ind_branch_predict(n->instr_addr, actual_dst));
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
   enum { 
      Ev_Ir,  // Instruction read
      Ev_Dr,  // Data read
      Ev_Dw,  // Data write
      Ev_Dm,  // Data modify (read then write)
      Ev_Bc,  // branch conditional
      Ev_Bi   // branch indirect (to unknown destination)
   }
   EventTag;

typedef
   struct {
      EventTag   tag;
      InstrInfo* inode;
      union {
         struct {
         } Ir;
         struct {
            IRAtom* ea;
            Int     szB;
         } Dr;
         struct {
            IRAtom* ea;
            Int     szB;
         } Dw;
         struct {
            IRAtom* ea;
            Int     szB;
         } Dm;
         struct {
            IRAtom* taken; /* :: Ity_I1 */
         } Bc;
         struct {
            IRAtom* dst;
         } Bi;
      } Ev;
   }
   Event;

static void init_Event ( Event* ev ) {
   VG_(memset)(ev, 0, sizeof(Event));
}

static IRAtom* get_Event_dea ( Event* ev ) {
   switch (ev->tag) {
      case Ev_Dr: return ev->Ev.Dr.ea;
      case Ev_Dw: return ev->Ev.Dw.ea;
      case Ev_Dm: return ev->Ev.Dm.ea;
      default:    tl_assert(0);
   }
}

static Int get_Event_dszB ( Event* ev ) {
   switch (ev->tag) {
      case Ev_Dr: return ev->Ev.Dr.szB;
      case Ev_Dw: return ev->Ev.Dw.szB;
      case Ev_Dm: return ev->Ev.Dm.szB;
      default:    tl_assert(0);
   }
}


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
      SB_info* sbInfo;

      /* Number InstrInfo bins 'used' so far. */
      Int sbInfo_i;

      /* The output SB being constructed. */
      IRSB* sbOut;
   }
   CgState;


/*------------------------------------------------------------*/
/*--- Instrumentation main                                 ---*/
/*------------------------------------------------------------*/

// Note that origAddr is the real origAddr, not the address of the first
// instruction in the block (they can be different due to redirection).
static
SB_info* get_SB_info(IRSB* sbIn, Addr origAddr)
{
   Int      i, n_instrs;
   IRStmt*  st;
   SB_info* sbInfo;

   // Count number of original instrs in SB
   n_instrs = 0;
   for (i = 0; i < sbIn->stmts_used; i++) {
      st = sbIn->stmts[i];
      if (Ist_IMark == st->tag) n_instrs++;
   }

   // Check that we don't have an entry for this BB in the instr-info table.
   // If this assertion fails, there has been some screwup:  some
   // translations must have been discarded but Cachegrind hasn't discarded
   // the corresponding entries in the instr-info table.
   sbInfo = VG_(OSetGen_Lookup)(instrInfoTable, &origAddr);
   tl_assert(NULL == sbInfo);

   // BB never translated before (at this address, at least;  could have
   // been unloaded and then reloaded elsewhere in memory)
   sbInfo = VG_(OSetGen_AllocNode)(instrInfoTable,
                                sizeof(SB_info) + n_instrs*sizeof(InstrInfo)); 
   sbInfo->SB_addr  = origAddr;
   sbInfo->n_instrs = n_instrs;
   VG_(OSetGen_Insert)( instrInfoTable, sbInfo );
   distinct_instrs++;

   return sbInfo;
}


static void showEvent ( Event* ev )
{
   switch (ev->tag) {
      case Ev_Ir: 
         VG_(printf)("Ir %p\n", ev->inode);
         break;
      case Ev_Dr:
         VG_(printf)("Dr %p %d EA=", ev->inode, ev->Ev.Dr.szB);
         ppIRExpr(ev->Ev.Dr.ea); 
         VG_(printf)("\n");
         break;
      case Ev_Dw:
         VG_(printf)("Dw %p %d EA=", ev->inode, ev->Ev.Dw.szB);
         ppIRExpr(ev->Ev.Dw.ea); 
         VG_(printf)("\n");
         break;
      case Ev_Dm:
         VG_(printf)("Dm %p %d EA=", ev->inode, ev->Ev.Dm.szB);
         ppIRExpr(ev->Ev.Dm.ea); 
         VG_(printf)("\n");
         break;
      case Ev_Bc:
         VG_(printf)("Bc %p   GA=", ev->inode);
         ppIRExpr(ev->Ev.Bc.taken); 
         VG_(printf)("\n");
         break;
      case Ev_Bi:
         VG_(printf)("Bi %p  DST=", ev->inode);
         ppIRExpr(ev->Ev.Bi.dst); 
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
   tl_assert(cgs->sbInfo_i >= 0);
   tl_assert(cgs->sbInfo_i < cgs->sbInfo->n_instrs);
   i_node = &cgs->sbInfo->instrs[ cgs->sbInfo_i ];
   i_node->instr_addr = instr_addr;
   i_node->instr_len  = instr_len;
   i_node->parent     = get_lineCC(instr_addr);
   cgs->sbInfo_i++;
   return i_node;
}


/* Generate code for all outstanding memory events, and mark the queue
   empty.  Code is generated into cgs->bbOut, and this activity
   'consumes' slots in cgs->sbInfo. */

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
      switch (ev->tag) {
         case Ev_Ir:
            /* Merge an Ir with a following Dr/Dm. */
            if (ev2 && (ev2->tag == Ev_Dr || ev2->tag == Ev_Dm)) {
               /* Why is this true?  It's because we're merging an Ir
                  with a following Dr or Dm.  The Ir derives from the
                  instruction's IMark and the Dr/Dm from data
                  references which follow it.  In short it holds
                  because each insn starts with an IMark, hence an
                  Ev_Ir, and so these Dr/Dm must pertain to the
                  immediately preceding Ir.  Same applies to analogous
                  assertions in the subsequent cases. */
               tl_assert(ev2->inode == ev->inode);
               helperName = "log_1I_1Dr_cache_access";
               helperAddr = &log_1I_1Dr_cache_access;
               argv = mkIRExprVec_3( i_node_expr,
                                     get_Event_dea(ev2),
                                     mkIRExpr_HWord( get_Event_dszB(ev2) ) );
               regparms = 3;
               i += 2;
            }
            /* Merge an Ir with a following Dw. */
            else
            if (ev2 && ev2->tag == Ev_Dw) {
               tl_assert(ev2->inode == ev->inode);
               helperName = "log_1I_1Dw_cache_access";
               helperAddr = &log_1I_1Dw_cache_access;
               argv = mkIRExprVec_3( i_node_expr,
                                     get_Event_dea(ev2),
                                     mkIRExpr_HWord( get_Event_dszB(ev2) ) );
               regparms = 3;
               i += 2;
            }
            /* Merge an Ir with two following Irs. */
            else
            if (ev2 && ev3 && ev2->tag == Ev_Ir && ev3->tag == Ev_Ir)
            {
               if (clo_cache_sim) {
                  helperName = "log_3I_0D_cache_access";
                  helperAddr = &log_3I_0D_cache_access;
               } else {
                  helperName = "log_3I";
                  helperAddr = &log_3I;
               }
               argv = mkIRExprVec_3( i_node_expr, 
                                     mkIRExpr_HWord( (HWord)ev2->inode ), 
                                     mkIRExpr_HWord( (HWord)ev3->inode ) );
               regparms = 3;
               i += 3;
            }
            /* Merge an Ir with one following Ir. */
            else
            if (ev2 && ev2->tag == Ev_Ir) {
               if (clo_cache_sim) {
                  helperName = "log_2I_0D_cache_access";
                  helperAddr = &log_2I_0D_cache_access;
               } else {
                  helperName = "log_2I";
                  helperAddr = &log_2I;
               }
               argv = mkIRExprVec_2( i_node_expr,
                                     mkIRExpr_HWord( (HWord)ev2->inode ) );
               regparms = 2;
               i += 2;
            }
            /* No merging possible; emit as-is. */
            else {
               if (clo_cache_sim) {
                  helperName = "log_1I_0D_cache_access";
                  helperAddr = &log_1I_0D_cache_access;
               } else {
                  helperName = "log_1I";
                  helperAddr = &log_1I;
               }
               argv = mkIRExprVec_1( i_node_expr );
               regparms = 1;
               i++;
            }
            break;
         case Ev_Dr:
         case Ev_Dm:
            /* Data read or modify */
            helperName = "log_0I_1Dr_cache_access";
            helperAddr = &log_0I_1Dr_cache_access;
            argv = mkIRExprVec_3( i_node_expr, 
                                  get_Event_dea(ev), 
                                  mkIRExpr_HWord( get_Event_dszB(ev) ) );
            regparms = 3;
            i++;
            break;
         case Ev_Dw:
            /* Data write */
            helperName = "log_0I_1Dw_cache_access";
            helperAddr = &log_0I_1Dw_cache_access;
            argv = mkIRExprVec_3( i_node_expr,
                                  get_Event_dea(ev), 
                                  mkIRExpr_HWord( get_Event_dszB(ev) ) );
            regparms = 3;
            i++;
            break;
         case Ev_Bc:
            /* Conditional branch */
            helperName = "log_cond_branch";
            helperAddr = &log_cond_branch;
            argv = mkIRExprVec_2( i_node_expr, ev->Ev.Bc.taken );
            regparms = 2;
            i++;
            break;
         case Ev_Bi:
            /* Branch to an unknown destination */
            helperName = "log_ind_branch";
            helperAddr = &log_ind_branch;
            argv = mkIRExprVec_2( i_node_expr, ev->Ev.Bi.dst );
            regparms = 2;
            i++;
            break;
         default:
            tl_assert(0);
      }

      /* Add the helper. */
      tl_assert(helperName);
      tl_assert(helperAddr);
      tl_assert(argv);
      di = unsafeIRDirty_0_N( regparms, 
                              helperName, VG_(fnptr_to_fnentry)( helperAddr ), 
                              argv );
      addStmtToIRSB( cgs->sbOut, IRStmt_Dirty(di) );
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
   init_Event(evt);
   evt->tag      = Ev_Ir;
   evt->inode    = inode;
   cgs->events_used++;
}

static
void addEvent_Dr ( CgState* cgs, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* evt;
   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= MIN_LINE_SIZE);
   if (!clo_cache_sim)
      return;
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   init_Event(evt);
   evt->tag       = Ev_Dr;
   evt->inode     = inode;
   evt->Ev.Dr.szB = datasize;
   evt->Ev.Dr.ea  = ea;
   cgs->events_used++;
}

static
void addEvent_Dw ( CgState* cgs, InstrInfo* inode, Int datasize, IRAtom* ea )
{
   Event* lastEvt;
   Event* evt;

   tl_assert(isIRAtom(ea));
   tl_assert(datasize >= 1 && datasize <= MIN_LINE_SIZE);

   if (!clo_cache_sim)
      return;

   /* Is it possible to merge this write with the preceding read? */
   lastEvt = &cgs->events[cgs->events_used-1];
   if (cgs->events_used > 0
    && lastEvt->tag       == Ev_Dr
    && lastEvt->Ev.Dr.szB == datasize
    && lastEvt->inode     == inode
    && eqIRAtom(lastEvt->Ev.Dr.ea, ea))
   {
      lastEvt->tag   = Ev_Dm;
      return;
   }

   /* No.  Add as normal. */
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   init_Event(evt);
   evt->tag       = Ev_Dw;
   evt->inode     = inode;
   evt->Ev.Dw.szB = datasize;
   evt->Ev.Dw.ea  = ea;
   cgs->events_used++;
}

static
void addEvent_Bc ( CgState* cgs, InstrInfo* inode, IRAtom* guard )
{
   Event* evt;
   tl_assert(isIRAtom(guard));
   tl_assert(typeOfIRExpr(cgs->sbOut->tyenv, guard) 
             == (sizeof(HWord)==4 ? Ity_I32 : Ity_I64));
   if (!clo_branch_sim)
      return;
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   init_Event(evt);
   evt->tag         = Ev_Bc;
   evt->inode       = inode;
   evt->Ev.Bc.taken = guard;
   cgs->events_used++;
}

static
void addEvent_Bi ( CgState* cgs, InstrInfo* inode, IRAtom* whereTo )
{
   Event* evt;
   tl_assert(isIRAtom(whereTo));
   tl_assert(typeOfIRExpr(cgs->sbOut->tyenv, whereTo) 
             == (sizeof(HWord)==4 ? Ity_I32 : Ity_I64));
   if (!clo_branch_sim)
      return;
   if (cgs->events_used == N_EVENTS)
      flushEvents(cgs);
   tl_assert(cgs->events_used >= 0 && cgs->events_used < N_EVENTS);
   evt = &cgs->events[cgs->events_used];
   init_Event(evt);
   evt->tag       = Ev_Bi;
   evt->inode     = inode;
   evt->Ev.Bi.dst = whereTo;
   cgs->events_used++;
}

////////////////////////////////////////////////////////////


static
IRSB* cg_instrument ( VgCallbackClosure* closure,
                      IRSB* sbIn, 
                      VexGuestLayout* layout, 
                      VexGuestExtents* vge,
                      IRType gWordTy, IRType hWordTy )
{
   Int        i, isize;
   IRStmt*    st;
   Addr64     cia; /* address of current insn */
   CgState    cgs;
   IRTypeEnv* tyenv = sbIn->tyenv;
   InstrInfo* curr_inode = NULL;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   // Set up new SB
   cgs.sbOut = deepCopyIRSBExceptStmts(sbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB( cgs.sbOut, sbIn->stmts[i] );
      i++;
   }

   // Get the first statement, and initial cia from it
   tl_assert(sbIn->stmts_used > 0);
   tl_assert(i < sbIn->stmts_used);
   st = sbIn->stmts[i];
   tl_assert(Ist_IMark == st->tag);

   cia   = st->Ist.IMark.addr;
   isize = st->Ist.IMark.len;
   // If Vex fails to decode an instruction, the size will be zero.
   // Pretend otherwise.
   if (isize == 0) isize = VG_MIN_INSTR_SZB;

   // Set up running state and get block info
   tl_assert(closure->readdr == vge->base[0]);
   cgs.events_used = 0;
   cgs.sbInfo      = get_SB_info(sbIn, (Addr)closure->readdr);
   cgs.sbInfo_i    = 0;

   if (DEBUG_CG)
      VG_(printf)("\n\n---------- cg_instrument ----------\n");

   // Traverse the block, initialising inodes, adding events and flushing as
   // necessary.
   for (/*use current i*/; i < sbIn->stmts_used; i++) {

      st = sbIn->stmts[i];
      tl_assert(isFlatIRStmt(st));

      switch (st->tag) {
         case Ist_NoOp:
         case Ist_AbiHint:
         case Ist_Put:
         case Ist_PutI:
         case Ist_MBE:
            break;

         case Ist_IMark:
            cia   = st->Ist.IMark.addr;
            isize = st->Ist.IMark.len;

            // If Vex fails to decode an instruction, the size will be zero.
            // Pretend otherwise.
            if (isize == 0) isize = VG_MIN_INSTR_SZB;

            // Sanity-check size.
            tl_assert( (VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB)
                     || VG_CLREQ_SZB == isize );

            // Get space for and init the inode, record it as the current one.
            // Subsequent Dr/Dw/Dm events from the same instruction will 
            // also use it.
            curr_inode = setup_InstrInfo(&cgs, cia, isize);

            addEvent_Ir( &cgs, curr_inode );
            break;

         case Ist_WrTmp: {
            IRExpr* data = st->Ist.WrTmp.data;
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

         case Ist_CAS: {
            /* We treat it as a read and a write of the location.  I
               think that is the same behaviour as it was before IRCAS
               was introduced, since prior to that point, the Vex
               front ends would translate a lock-prefixed instruction
               into a (normal) read followed by a (normal) write. */
            Int    dataSize;
            IRCAS* cas = st->Ist.CAS.details;
            tl_assert(cas->addr != NULL);
            tl_assert(cas->dataLo != NULL);
            dataSize = sizeofIRType(typeOfIRExpr(tyenv, cas->dataLo));
            if (cas->dataHi != NULL)
               dataSize *= 2; /* since it's a doubleword-CAS */
            /* I don't think this can ever happen, but play safe. */
            if (dataSize > MIN_LINE_SIZE)
               dataSize = MIN_LINE_SIZE;
            addEvent_Dr( &cgs, curr_inode, dataSize, cas->addr );
            addEvent_Dw( &cgs, curr_inode, dataSize, cas->addr );
            break;
         }

         case Ist_LLSC: {
            IRType dataTy;
            if (st->Ist.LLSC.storedata == NULL) {
               /* LL */
               dataTy = typeOfIRTemp(tyenv, st->Ist.LLSC.result);
               addEvent_Dr( &cgs, curr_inode,
                            sizeofIRType(dataTy), st->Ist.LLSC.addr );
            } else {
               /* SC */
               dataTy = typeOfIRExpr(tyenv, st->Ist.LLSC.storedata);
               addEvent_Dw( &cgs, curr_inode,
                            sizeofIRType(dataTy), st->Ist.LLSC.addr );
            }
            break;
         }

         case Ist_Exit: {
            // call branch predictor only if this is a branch in guest code
            if ( (st->Ist.Exit.jk == Ijk_Boring) ||
                 (st->Ist.Exit.jk == Ijk_Call) ||
                 (st->Ist.Exit.jk == Ijk_Ret) )
            {
               /* Stuff to widen the guard expression to a host word, so
                  we can pass it to the branch predictor simulation
                  functions easily. */
               Bool     inverted;
               Addr64   nia, sea;
               IRConst* dst;
               IRType   tyW    = hWordTy;
               IROp     widen  = tyW==Ity_I32  ? Iop_1Uto32  : Iop_1Uto64;
               IROp     opXOR  = tyW==Ity_I32  ? Iop_Xor32   : Iop_Xor64;
               IRTemp   guard1 = newIRTemp(cgs.sbOut->tyenv, Ity_I1);
               IRTemp   guardW = newIRTemp(cgs.sbOut->tyenv, tyW);
               IRTemp   guard  = newIRTemp(cgs.sbOut->tyenv, tyW);
               IRExpr*  one    = tyW==Ity_I32 ? IRExpr_Const(IRConst_U32(1))
                                              : IRExpr_Const(IRConst_U64(1));

               /* First we need to figure out whether the side exit got
                  inverted by the ir optimiser.  To do that, figure out
                  the next (fallthrough) instruction's address and the
                  side exit address and see if they are the same. */
               nia = cia + (Addr64)isize;
               if (tyW == Ity_I32)
                  nia &= 0xFFFFFFFFULL;

               /* Side exit address */
               dst = st->Ist.Exit.dst;
               if (tyW == Ity_I32) {
                  tl_assert(dst->tag == Ico_U32);
                  sea = (Addr64)(UInt)dst->Ico.U32;
               } else {
                  tl_assert(tyW == Ity_I64);
                  tl_assert(dst->tag == Ico_U64);
                  sea = dst->Ico.U64;
               }

               inverted = nia == sea;

               /* Widen the guard expression. */
               addStmtToIRSB( cgs.sbOut,
                              IRStmt_WrTmp( guard1, st->Ist.Exit.guard ));
               addStmtToIRSB( cgs.sbOut,
                              IRStmt_WrTmp( guardW,
                                            IRExpr_Unop(widen,
                                                        IRExpr_RdTmp(guard1))) );
               /* If the exit is inverted, invert the sense of the guard. */
               addStmtToIRSB(
                     cgs.sbOut,
                     IRStmt_WrTmp(
                           guard,
                           inverted ? IRExpr_Binop(opXOR, IRExpr_RdTmp(guardW), one)
                                    : IRExpr_RdTmp(guardW)
                              ));
               /* And post the event. */
               addEvent_Bc( &cgs, curr_inode, IRExpr_RdTmp(guard) );
            }

            /* We may never reach the next statement, so need to flush
               all outstanding transactions now. */
            flushEvents( &cgs );
            break;
         }

         default:
            tl_assert(0);
            break;
      }

      /* Copy the original statement */
      addStmtToIRSB( cgs.sbOut, st );

      if (DEBUG_CG) {
         ppIRStmt(st);
         VG_(printf)("\n");
      }
   }

   /* Deal with branches to unknown destinations.  Except ignore ones
      which are function returns as we assume the return stack
      predictor never mispredicts. */
   if ((sbIn->jumpkind == Ijk_Boring) || (sbIn->jumpkind == Ijk_Call)) {
      if (0) { ppIRExpr( sbIn->next ); VG_(printf)("\n"); }
      switch (sbIn->next->tag) {
         case Iex_Const: 
            break; /* boring - branch to known address */
         case Iex_RdTmp: 
            /* looks like an indirect branch (branch to unknown) */
            addEvent_Bi( &cgs, curr_inode, sbIn->next );
            break;
         default:
            /* shouldn't happen - if the incoming IR is properly
               flattened, should only have tmp and const cases to
               consider. */
            tl_assert(0); 
      }
   }

   /* At the end of the bb.  Flush outstandings. */
   flushEvents( &cgs );

   /* done.  stay sane ... */
   tl_assert(cgs.sbInfo_i == cgs.sbInfo->n_instrs);

   if (DEBUG_CG) {
      VG_(printf)( "goto {");
      ppIRJumpKind(sbIn->jumpkind);
      VG_(printf)( "} ");
      ppIRExpr( sbIn->next );
      VG_(printf)( "}\n");
   }

   return cgs.sbOut;
}

/*------------------------------------------------------------*/
/*--- Cache configuration                                  ---*/
/*------------------------------------------------------------*/

#define UNDEFINED_CACHE     { -1, -1, -1 }

static cache_t clo_I1_cache = UNDEFINED_CACHE;
static cache_t clo_D1_cache = UNDEFINED_CACHE;
static cache_t clo_LL_cache = UNDEFINED_CACHE;

/*------------------------------------------------------------*/
/*--- cg_fini() and related function                       ---*/
/*------------------------------------------------------------*/

// Total reads/writes/misses.  Calculated during CC traversal at the end.
// All auto-zeroed.
static CacheCC  Ir_total;
static CacheCC  Dr_total;
static CacheCC  Dw_total;
static BranchCC Bc_total;
static BranchCC Bi_total;

static void fprint_CC_table_and_calc_totals(void)
{
   Int     i, fd;
   SysRes  sres;
   Char    buf[512], *currFile = NULL, *currFn = NULL;
   LineCC* lineCC;

   // Setup output filename.  Nb: it's important to do this now, ie. as late
   // as possible.  If we do it at start-up and the program forks and the
   // output file format string contains a %p (pid) specifier, both the
   // parent and child will incorrectly write to the same file;  this
   // happened in 3.3.0.
   Char* cachegrind_out_file =
      VG_(expand_file_name)("--cachegrind-out-file", clo_cachegrind_out_file);

   sres = VG_(open)(cachegrind_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                                         VKI_S_IRUSR|VKI_S_IWUSR);
   if (sr_isError(sres)) {
      // If the file can't be opened for whatever reason (conflict
      // between multiple cachegrinded processes?), give up now.
      VG_(umsg)("error: can't open cache simulation output file '%s'\n",
                cachegrind_out_file );
      VG_(umsg)("       ... so simulation results will be missing.\n");
      VG_(free)(cachegrind_out_file);
      return;
   } else {
      fd = sr_Res(sres);
      VG_(free)(cachegrind_out_file);
   }

   // "desc:" lines (giving I1/D1/LL cache configuration).  The spaces after
   // the 2nd colon makes cg_annotate's output look nicer.
   VG_(sprintf)(buf, "desc: I1 cache:         %s\n"
                     "desc: D1 cache:         %s\n"
                     "desc: LL cache:         %s\n",
                     I1.desc_line, D1.desc_line, LL.desc_line);
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   // "cmd:" line
   VG_(strcpy)(buf, "cmd:");
   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
   if (VG_(args_the_exename)) {
      VG_(write)(fd, " ", 1);
      VG_(write)(fd, VG_(args_the_exename), 
                     VG_(strlen)( VG_(args_the_exename) ));
   }
   for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      HChar* arg = * (HChar**) VG_(indexXA)( VG_(args_for_client), i );
      if (arg) {
         VG_(write)(fd, " ", 1);
         VG_(write)(fd, arg, VG_(strlen)( arg ));
      }
   }
   // "events:" line
   if (clo_cache_sim && clo_branch_sim) {
      VG_(sprintf)(buf, "\nevents: Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw "
                                  "Bc Bcm Bi Bim\n");
   }
   else if (clo_cache_sim && !clo_branch_sim) {
      VG_(sprintf)(buf, "\nevents: Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw "
                                  "\n");
   }
   else if (!clo_cache_sim && clo_branch_sim) {
      VG_(sprintf)(buf, "\nevents: Ir "
                                  "Bc Bcm Bi Bim\n");
   }
   else {
      VG_(sprintf)(buf, "\nevents: Ir\n");
   }

   VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

   // Traverse every lineCC
   VG_(OSetGen_ResetIter)(CC_table);
   while ( (lineCC = VG_(OSetGen_Next)(CC_table)) ) {
      Bool just_hit_a_new_file = False;
      // If we've hit a new file, print a "fl=" line.  Note that because
      // each string is stored exactly once in the string table, we can use
      // pointer comparison rather than strcmp() to test for equality, which
      // is good because most of the time the comparisons are equal and so
      // the whole strings would have to be checked.
      if ( lineCC->loc.file != currFile ) {
         currFile = lineCC->loc.file;
         VG_(sprintf)(buf, "fl=%s\n", currFile);
         VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
         distinct_files++;
         just_hit_a_new_file = True;
      }
      // If we've hit a new function, print a "fn=" line.  We know to do
      // this when the function name changes, and also every time we hit a
      // new file (in which case the new function name might be the same as
      // in the old file, hence the just_hit_a_new_file test).
      if ( just_hit_a_new_file || lineCC->loc.fn != currFn ) {
         currFn = lineCC->loc.fn;
         VG_(sprintf)(buf, "fn=%s\n", currFn);
         VG_(write)(fd, (void*)buf, VG_(strlen)(buf));
         distinct_fns++;
      }

      // Print the LineCC
      if (clo_cache_sim && clo_branch_sim) {
         VG_(sprintf)(buf, "%u %llu %llu %llu"
                             " %llu %llu %llu"
                             " %llu %llu %llu"
                             " %llu %llu %llu %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a, lineCC->Ir.m1, lineCC->Ir.mL, 
                            lineCC->Dr.a, lineCC->Dr.m1, lineCC->Dr.mL,
                            lineCC->Dw.a, lineCC->Dw.m1, lineCC->Dw.mL,
                            lineCC->Bc.b, lineCC->Bc.mp, 
                            lineCC->Bi.b, lineCC->Bi.mp);
      }
      else if (clo_cache_sim && !clo_branch_sim) {
         VG_(sprintf)(buf, "%u %llu %llu %llu"
                             " %llu %llu %llu"
                             " %llu %llu %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a, lineCC->Ir.m1, lineCC->Ir.mL, 
                            lineCC->Dr.a, lineCC->Dr.m1, lineCC->Dr.mL,
                            lineCC->Dw.a, lineCC->Dw.m1, lineCC->Dw.mL);
      }
      else if (!clo_cache_sim && clo_branch_sim) {
         VG_(sprintf)(buf, "%u %llu"
                             " %llu %llu %llu %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a, 
                            lineCC->Bc.b, lineCC->Bc.mp, 
                            lineCC->Bi.b, lineCC->Bi.mp);
      }
      else {
         VG_(sprintf)(buf, "%u %llu\n",
                            lineCC->loc.line,
                            lineCC->Ir.a);
      }

      VG_(write)(fd, (void*)buf, VG_(strlen)(buf));

      // Update summary stats
      Ir_total.a  += lineCC->Ir.a;
      Ir_total.m1 += lineCC->Ir.m1;
      Ir_total.mL += lineCC->Ir.mL;
      Dr_total.a  += lineCC->Dr.a;
      Dr_total.m1 += lineCC->Dr.m1;
      Dr_total.mL += lineCC->Dr.mL;
      Dw_total.a  += lineCC->Dw.a;
      Dw_total.m1 += lineCC->Dw.m1;
      Dw_total.mL += lineCC->Dw.mL;
      Bc_total.b  += lineCC->Bc.b;
      Bc_total.mp += lineCC->Bc.mp;
      Bi_total.b  += lineCC->Bi.b;
      Bi_total.mp += lineCC->Bi.mp;

      distinct_lines++;
   }

   // Summary stats must come after rest of table, since we calculate them
   // during traversal.  */
   if (clo_cache_sim && clo_branch_sim) {
      VG_(sprintf)(buf, "summary:"
                        " %llu %llu %llu"
                        " %llu %llu %llu"
                        " %llu %llu %llu"
                        " %llu %llu %llu %llu\n", 
                        Ir_total.a, Ir_total.m1, Ir_total.mL,
                        Dr_total.a, Dr_total.m1, Dr_total.mL,
                        Dw_total.a, Dw_total.m1, Dw_total.mL,
                        Bc_total.b, Bc_total.mp, 
                        Bi_total.b, Bi_total.mp);
   }
   else if (clo_cache_sim && !clo_branch_sim) {
      VG_(sprintf)(buf, "summary:"
                        " %llu %llu %llu"
                        " %llu %llu %llu"
                        " %llu %llu %llu\n",
                        Ir_total.a, Ir_total.m1, Ir_total.mL,
                        Dr_total.a, Dr_total.m1, Dr_total.mL,
                        Dw_total.a, Dw_total.m1, Dw_total.mL);
   }
   else if (!clo_cache_sim && clo_branch_sim) {
      VG_(sprintf)(buf, "summary:"
                        " %llu"
                        " %llu %llu %llu %llu\n", 
                        Ir_total.a,
                        Bc_total.b, Bc_total.mp, 
                        Bi_total.b, Bi_total.mp);
   }
   else {
      VG_(sprintf)(buf, "summary:"
                        " %llu\n", 
                        Ir_total.a);
   }

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
   if (w == 0) w = 1;
   return w + (w-1)/3;   // add space for commas
}

static void cg_fini(Int exitcode)
{
   static Char buf1[128], buf2[128], buf3[128], buf4[123], fmt[128];

   CacheCC  D_total;
   BranchCC B_total;
   ULong LL_total_m, LL_total_mr, LL_total_mw,
         LL_total, LL_total_r, LL_total_w;
   Int l1, l2, l3;

   fprint_CC_table_and_calc_totals();

   if (VG_(clo_verbosity) == 0) 
      return;

   // Nb: this isn't called "MAX" because that overshadows a global on Darwin.
   #define CG_MAX(a, b)  ((a) >= (b) ? (a) : (b))

   /* I cache results.  Use the I_refs value to determine the first column
    * width. */
   l1 = ULong_width(Ir_total.a);
   l2 = ULong_width(CG_MAX(Dr_total.a, Bc_total.b));
   l3 = ULong_width(CG_MAX(Dw_total.a, Bi_total.b));

   /* Make format string, getting width right for numbers */
   VG_(sprintf)(fmt, "%%s %%,%dllu\n", l1);

   /* Always print this */
   VG_(umsg)(fmt, "I   refs:     ", Ir_total.a);

   /* If cache profiling is enabled, show D access numbers and all
      miss numbers */
   if (clo_cache_sim) {
      VG_(umsg)(fmt, "I1  misses:   ", Ir_total.m1);
      VG_(umsg)(fmt, "LLi misses:   ", Ir_total.mL);

      if (0 == Ir_total.a) Ir_total.a = 1;
      VG_(percentify)(Ir_total.m1, Ir_total.a, 2, l1+1, buf1);
      VG_(umsg)("I1  miss rate: %s\n", buf1);

      VG_(percentify)(Ir_total.mL, Ir_total.a, 2, l1+1, buf1);
      VG_(umsg)("LLi miss rate: %s\n", buf1);
      VG_(umsg)("\n");

      /* D cache results.  Use the D_refs.rd and D_refs.wr values to
       * determine the width of columns 2 & 3. */
      D_total.a  = Dr_total.a  + Dw_total.a;
      D_total.m1 = Dr_total.m1 + Dw_total.m1;
      D_total.mL = Dr_total.mL + Dw_total.mL;

      /* Make format string, getting width right for numbers */
      VG_(sprintf)(fmt, "%%s %%,%dllu  (%%,%dllu rd   + %%,%dllu wr)\n",
                        l1, l2, l3);

      VG_(umsg)(fmt, "D   refs:     ", 
                     D_total.a, Dr_total.a, Dw_total.a);
      VG_(umsg)(fmt, "D1  misses:   ",
                     D_total.m1, Dr_total.m1, Dw_total.m1);
      VG_(umsg)(fmt, "LLd misses:   ",
                     D_total.mL, Dr_total.mL, Dw_total.mL);

      if (0 == D_total.a)  D_total.a = 1;
      if (0 == Dr_total.a) Dr_total.a = 1;
      if (0 == Dw_total.a) Dw_total.a = 1;
      VG_(percentify)( D_total.m1,  D_total.a, 1, l1+1, buf1);
      VG_(percentify)(Dr_total.m1, Dr_total.a, 1, l2+1, buf2);
      VG_(percentify)(Dw_total.m1, Dw_total.a, 1, l3+1, buf3);
      VG_(umsg)("D1  miss rate: %s (%s     + %s  )\n", buf1, buf2,buf3);

      VG_(percentify)( D_total.mL,  D_total.a, 1, l1+1, buf1);
      VG_(percentify)(Dr_total.mL, Dr_total.a, 1, l2+1, buf2);
      VG_(percentify)(Dw_total.mL, Dw_total.a, 1, l3+1, buf3);
      VG_(umsg)("LLd miss rate: %s (%s     + %s  )\n", buf1, buf2,buf3);
      VG_(umsg)("\n");

      /* LL overall results */

      LL_total   = Dr_total.m1 + Dw_total.m1 + Ir_total.m1;
      LL_total_r = Dr_total.m1 + Ir_total.m1;
      LL_total_w = Dw_total.m1;
      VG_(umsg)(fmt, "LL refs:      ",
                     LL_total, LL_total_r, LL_total_w);

      LL_total_m  = Dr_total.mL + Dw_total.mL + Ir_total.mL;
      LL_total_mr = Dr_total.mL + Ir_total.mL;
      LL_total_mw = Dw_total.mL;
      VG_(umsg)(fmt, "LL misses:    ",
                     LL_total_m, LL_total_mr, LL_total_mw);

      VG_(percentify)(LL_total_m,  (Ir_total.a + D_total.a),  1, l1+1, buf1);
      VG_(percentify)(LL_total_mr, (Ir_total.a + Dr_total.a), 1, l2+1, buf2);
      VG_(percentify)(LL_total_mw, Dw_total.a,                1, l3+1, buf3);
      VG_(umsg)("LL miss rate:  %s (%s     + %s  )\n", buf1, buf2,buf3);
   }

   /* If branch profiling is enabled, show branch overall results. */
   if (clo_branch_sim) {
      /* Make format string, getting width right for numbers */
      VG_(sprintf)(fmt, "%%s %%,%dllu  (%%,%dllu cond + %%,%dllu ind)\n",
                        l1, l2, l3);

      if (0 == Bc_total.b)  Bc_total.b = 1;
      if (0 == Bi_total.b)  Bi_total.b = 1;
      B_total.b  = Bc_total.b  + Bi_total.b;
      B_total.mp = Bc_total.mp + Bi_total.mp;

      VG_(umsg)("\n");
      VG_(umsg)(fmt, "Branches:     ",
                     B_total.b, Bc_total.b, Bi_total.b);

      VG_(umsg)(fmt, "Mispredicts:  ",
                     B_total.mp, Bc_total.mp, Bi_total.mp);

      VG_(percentify)(B_total.mp,  B_total.b,  1, l1+1, buf1);
      VG_(percentify)(Bc_total.mp, Bc_total.b, 1, l2+1, buf2);
      VG_(percentify)(Bi_total.mp, Bi_total.b, 1, l3+1, buf3);

      VG_(umsg)("Mispred rate:  %s (%s     + %s   )\n", buf1, buf2,buf3);
   }

   // Various stats
   if (VG_(clo_stats)) {
      Int debug_lookups = full_debugs      + fn_debugs +
                          file_line_debugs + no_debugs;

      VG_(dmsg)("\n");
      VG_(dmsg)("cachegrind: distinct files: %d\n", distinct_files);
      VG_(dmsg)("cachegrind: distinct fns:   %d\n", distinct_fns);
      VG_(dmsg)("cachegrind: distinct lines: %d\n", distinct_lines);
      VG_(dmsg)("cachegrind: distinct instrs:%d\n", distinct_instrs);
      VG_(dmsg)("cachegrind: debug lookups      : %d\n", debug_lookups);
      
      VG_(percentify)(full_debugs,      debug_lookups, 1, 6, buf1);
      VG_(percentify)(file_line_debugs, debug_lookups, 1, 6, buf2);
      VG_(percentify)(fn_debugs,        debug_lookups, 1, 6, buf3);
      VG_(percentify)(no_debugs,        debug_lookups, 1, 6, buf4);
      VG_(dmsg)("cachegrind: with full      info:%s (%d)\n", 
                buf1, full_debugs);
      VG_(dmsg)("cachegrind: with file/line info:%s (%d)\n", 
                buf2, file_line_debugs);
      VG_(dmsg)("cachegrind: with fn name   info:%s (%d)\n", 
                buf3, fn_debugs);
      VG_(dmsg)("cachegrind: with zero      info:%s (%d)\n", 
                buf4, no_debugs);

      VG_(dmsg)("cachegrind: string table size: %lu\n",
                VG_(OSetGen_Size)(stringTable));
      VG_(dmsg)("cachegrind: CC table size: %lu\n",
                VG_(OSetGen_Size)(CC_table));
      VG_(dmsg)("cachegrind: InstrInfo table size: %lu\n",
                VG_(OSetGen_Size)(instrInfoTable));
   }
}

/*--------------------------------------------------------------------*/
/*--- Discarding BB info                                           ---*/
/*--------------------------------------------------------------------*/

// Called when a translation is removed from the translation cache for
// any reason at all: to free up space, because the guest code was
// unmapped or modified, or for any arbitrary reason.
static
void cg_discard_superblock_info ( Addr64 orig_addr64, VexGuestExtents vge )
{
   SB_info* sbInfo;
   Addr     orig_addr = (Addr)vge.base[0];

   tl_assert(vge.n_used > 0);

   if (DEBUG_CG)
      VG_(printf)( "discard_basic_block_info: %p, %p, %llu\n", 
                   (void*)(Addr)orig_addr,
                   (void*)(Addr)vge.base[0], (ULong)vge.len[0]);

   // Get BB info, remove from table, free BB info.  Simple!  Note that we
   // use orig_addr, not the first instruction address in vge.
   sbInfo = VG_(OSetGen_Remove)(instrInfoTable, &orig_addr);
   tl_assert(NULL != sbInfo);
   VG_(OSetGen_FreeNode)(instrInfoTable, sbInfo);
}

/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

static Bool cg_process_cmd_line_option(Char* arg)
{
   if (VG_(str_clo_cache_opt)(arg,
                              &clo_I1_cache,
                              &clo_D1_cache,
                              &clo_LL_cache)) {}

   else if VG_STR_CLO( arg, "--cachegrind-out-file", clo_cachegrind_out_file) {}
   else if VG_BOOL_CLO(arg, "--cache-sim",  clo_cache_sim)  {}
   else if VG_BOOL_CLO(arg, "--branch-sim", clo_branch_sim) {}
   else
      return False;

   return True;
}

static void cg_print_usage(void)
{
   VG_(print_cache_clo_opts)();
   VG_(printf)(
"    --cache-sim=yes|no  [yes]        collect cache stats?\n"
"    --branch-sim=yes|no [no]         collect branch prediction stats?\n"
"    --cachegrind-out-file=<file>     output file name [cachegrind.out.%%p]\n"
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

static void cg_post_clo_init(void); /* just below */

static void cg_pre_clo_init(void)
{
   VG_(details_name)            ("Cachegrind");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a cache and branch-prediction profiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2011, and GNU GPL'd, by Nicholas Nethercote et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 500 );

   VG_(basic_tool_funcs)          (cg_post_clo_init,
                                   cg_instrument,
                                   cg_fini);

   VG_(needs_superblock_discards)(cg_discard_superblock_info);
   VG_(needs_command_line_options)(cg_process_cmd_line_option,
                                   cg_print_usage,
                                   cg_print_debug_usage);
}

static void cg_post_clo_init(void)
{
   cache_t I1c, D1c, LLc; 

   CC_table =
      VG_(OSetGen_Create)(offsetof(LineCC, loc),
                          cmp_CodeLoc_LineCC,
                          VG_(malloc), "cg.main.cpci.1",
                          VG_(free));
   instrInfoTable =
      VG_(OSetGen_Create)(/*keyOff*/0,
                          NULL,
                          VG_(malloc), "cg.main.cpci.2",
                          VG_(free));
   stringTable =
      VG_(OSetGen_Create)(/*keyOff*/0,
                          stringCmp,
                          VG_(malloc), "cg.main.cpci.3",
                          VG_(free));

   VG_(post_clo_init_configure_caches)(&I1c, &D1c, &LLc,
                                       &clo_I1_cache,
                                       &clo_D1_cache,
                                       &clo_LL_cache);

   cachesim_I1_initcache(I1c);
   cachesim_D1_initcache(D1c);
   cachesim_LL_initcache(LLc);
}

VG_DETERMINE_INTERFACE_VERSION(cg_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

