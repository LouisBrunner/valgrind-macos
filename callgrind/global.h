/*--------------------------------------------------------------------*/
/*--- Callgrind data structures, functions.               global.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 Josef Weidendorfer
      josef.weidendorfer@gmx.de

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef CLG_GLOBAL
#define CLG_GLOBAL

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
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_machine.h"      // VG_(fnptr_to_fnentry)

#include "events.h" // defines CLG_ macro
#include "costs.h"


/*------------------------------------------------------------*/
/*--- Callgrind compile options                           --- */
/*------------------------------------------------------------*/

/* Enable debug output */
#define CLG_ENABLE_DEBUG 1

/* Enable experimental features? */
#define CLG_EXPERIMENTAL 0


/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

#define DEFAULT_OUTFORMAT   "callgrind.out.%p"

/* If and how to collect syscall time.
   systime_no : do not collect systime
   systime_msec : collect syscount, systime elapsed, milli second precision.
   systime_usec : collect syscount, systime elapsed, micro second precision.
   systime_nsec : collect syscount, systime elapsed, systime cpu, nano second
                  precision.  */
typedef enum {
   systime_no,
   systime_msec,
   systime_usec,
   systime_nsec
} Collect_Systime;

typedef struct _CommandLineOptions CommandLineOptions;
struct _CommandLineOptions {

  /* Dump format options */
  const HChar* out_format;  /* Format string for callgrind output file name */
  Bool combine_dumps;       /* Dump trace parts into same file? */
  Bool compress_strings;
  Bool compress_events;
  Bool compress_pos;
  Bool mangle_names;
  Bool compress_mangled;
  Bool dump_line;
  Bool dump_instr;
  Bool dump_bb;
  Bool dump_bbs;         /* Dump basic block information? */
  
  /* Dump generation options */
  ULong dump_every_bb;     /* Dump every xxx BBs. */
  
  /* Collection options */
  Bool separate_threads; /* Separate threads in dump? */
  Int  separate_callers; /* Separate dependent on how many callers? */
  Int  separate_recursions; /* Max level of recursions to separate */
  Bool skip_plt;         /* Skip functions in PLT section? */
  Bool skip_direct_recursion; /* Increment direct recursions the level? */

  Bool collect_atstart;  /* Start in collecting state ? */
  Bool collect_jumps;    /* Collect (cond.) jumps in functions ? */

  Bool collect_alloc;    /* Collect size of allocated memory */
  Collect_Systime collect_systime;  /* Collect time for system calls */

  Bool collect_bus;      /* Collect global bus events */

  /* Instrument options */
  Bool instrument_atstart;  /* Instrument at start? */
  Bool simulate_cache;      /* Call into cache simulator ? */
  Bool simulate_branch;     /* Call into branch prediction simulator ? */

  /* Call graph generation */
  Bool pop_on_jump;       /* Handle a jump between functions as ret+call */

#if CLG_ENABLE_DEBUG
  Int   verbose;
  ULong verbose_start;
#endif
};

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

/* Minimum cache line size allowed */
#define MIN_LINE_SIZE   16


/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/

typedef struct _Statistics Statistics;
struct _Statistics {
  ULong call_counter;
  ULong jcnd_counter;
  ULong jump_counter;
  ULong rec_call_counter;
  ULong ret_counter;
  ULong bb_executions;

  Int  context_counter;
  Int  bb_retranslations;  

  Int  distinct_objs;
  Int  distinct_files;
  Int  distinct_fns;
  Int  distinct_contexts;
  Int  distinct_bbs;
  Int  distinct_jccs;
  Int  distinct_bbccs;
  Int  distinct_instrs;
  Int  distinct_skips;

  Int  bb_hash_resizes;
  Int  bbcc_hash_resizes;
  Int  jcc_hash_resizes;
  Int  cxt_hash_resizes;
  Int  fn_array_resizes;
  Int  call_stack_resizes;
  Int  fn_stack_resizes;

  Int  full_debug_BBs;
  Int  file_line_debug_BBs;
  Int  fn_name_debug_BBs;
  Int  no_debug_BBs;
  Int  bbcc_lru_misses;
  Int  jcc_lru_misses;
  Int  cxt_lru_misses;
  Int  bbcc_clones;
};


/*------------------------------------------------------------*/
/*--- Structure declarations                               ---*/
/*------------------------------------------------------------*/

typedef struct _Context     Context;
typedef struct _CC          CC;
typedef struct _BB          BB;
typedef struct _BBCC        BBCC;
typedef struct _jCC         jCC;
typedef struct _fCC         fCC;
typedef struct _fn_node     fn_node;
typedef struct _file_node   file_node;
typedef struct _obj_node    obj_node;
typedef struct _fn_config   fn_config;
typedef struct _call_entry  call_entry;
typedef struct _thread_info thread_info;

/* Costs of event sets. Aliases to arrays of 64-bit values */
typedef ULong* SimCost;  /* All events the simulator can produce */
typedef ULong* UserCost;
typedef ULong* FullCost; /* Simulator + User */


/* The types of control flow changes that can happen between
 * execution of two BBs in a thread.
 */
typedef enum {
  jk_None = 0,   /* no explicit change by a guest instruction */
  jk_Jump,       /* regular jump */
  jk_Call,
  jk_Return,
  jk_CondJump    /* conditional jump taken (only used as jCC type) */
} ClgJumpKind;


/* JmpCall cost center
 * for subroutine call (from->bb->jmp_addr => to->bb->addr)
 *
 * Each BB has at most one CALL instruction. The list of JCC from
 * this call is a pointer to the list head (stored in BBCC), and
 * <next_from> in the JCC struct.
 *
 * For fast lookup, JCCs are reachable with a hash table, keyed by
 * the (from_bbcc,to) pair. <next_hash> is used for the JCC chain
 * of one hash table entry.
 *
 * Cost <sum> holds event counts for already returned executions.
 * <last> are the event counters at last enter of the subroutine.
 * <sum> is updated on returning from the subroutine by
 * adding the diff of <last> and current event counters to <sum>.
 *
 * After updating, <last> is set to current event counters. Thus,
 * events are not counted twice for recursive calls (TODO: True?)
 */

struct _jCC {
  ClgJumpKind jmpkind; /* jk_Call, jk_Jump, jk_CondJump */
  jCC* next_hash;   /* for hash entry chain */
  jCC* next_from;   /* next JCC from a BBCC */
  BBCC *from, *to;  /* call arc from/to this BBCC */
  UInt jmp;         /* jump no. in source */

  ULong call_counter; /* no wraparound with 64 bit */

  FullCost cost; /* simulator + user counters */
};


/* 
 * Info for one instruction of a basic block.
 */
typedef struct _InstrInfo InstrInfo;
struct _InstrInfo {
  UInt instr_offset;
  UInt instr_size;
  UInt cost_offset;
  EventSet* eventset;
};



/*
 * Info for a side exit in a BB
 */
typedef struct _CJmpInfo CJmpInfo;
struct _CJmpInfo {
  UInt instr;          /* instruction index for BB.instr array */
  ClgJumpKind jmpkind; /* jump kind when leaving BB at this side exit */
};


/**
 * An instrumented basic block (BB).
 *
 * BBs are put into a resizable hash to allow for fast detection if a
 * BB is to be retranslated but cost info is already available.
 * The key for a BB is a (object, offset) tupel making it independent
 * from possibly multiple mappings of the same ELF object.
 *
 * At the beginning of each instrumented BB,
 * a call to setup_bbcc(), specifying a pointer to the
 * according BB structure, is added.
 *
 * As cost of a BB has to be distinguished depending on the context,
 * multiple cost centers for one BB (struct BBCC) exist and the according
 * BBCC is set by setup_bbcc.
 */
struct _BB {
  obj_node*  obj;         /* ELF object of BB */
  PtrdiffT   offset;      /* offset of BB in ELF object file */
  BB*        next;       /* chaining for a hash entry */

  VgSectKind sect_kind;  /* section of this BB, e.g. PLT */
  UInt       instr_count;
  
  /* filled by CLG_(get_fn_node) if debug info is available */
  fn_node*   fn;          /* debug info for this BB */
  UInt       line;
  Bool       is_entry;    /* True if this BB is a function entry */
        
  BBCC*      bbcc_list;  /* BBCCs for same BB (see next_bbcc in BBCC) */
  BBCC*      last_bbcc;  /* Temporary: Cached for faster access (LRU) */

  /* filled by CLG_(instrument) if not seen before */
  UInt       cjmp_count;  /* number of side exits */
  CJmpInfo*  jmp;         /* array of info for condition jumps,
			   * allocated directly after this struct */
  Bool       cjmp_inverted; /* is last side exit actually fall through? */

  UInt       instr_len;
  UInt       cost_count;
  InstrInfo  instr[0];   /* info on instruction sizes and costs */
};



/**
 * Function context
 *
 * Basic blocks are always executed in the scope of a context.
 * A function context is a list of function nodes representing
 * the call chain to the current context: I.e. fn[0] is the
 * function we are currently in, fn[1] has called fn[0], and so on.
 * Recursion levels are used for fn[0].
 *
 * To get a unique number for a full execution context, use
 *  rec_index = min(<fn->rec_separation>,<active>) - 1;
 *  unique_no = <number> + rec_index
 *
 * For each Context, recursion index and BB, there can be a BBCC.
 */
struct _Context {
    UInt size;        // number of function dependencies
    UInt base_number; // for context compression & dump array
    Context* next;    // entry chaining for hash
    UWord hash;       // for faster lookup...
    fn_node* fn[0];
};


/*
 * Cost info for a side exits from a BB
 */
typedef struct _JmpData JmpData;
struct _JmpData {
    ULong ecounter; /* number of times the BB was left at this exit */
    jCC*  jcc_list; /* JCCs used for this exit */
};


/*
 * Basic Block Cost Center
 *
 * On demand, multiple BBCCs will be created for the same BB
 * dependent on command line options and:
 * - current function (it's possible that a BB is executed in the
 *   context of different functions, e.g. in manual assembler/PLT)
 * - current thread ID
 * - position where current function is called from
 * - recursion level of current function
 *
 * The cost centres for the instructions of a basic block are
 * stored in a contiguous array.
 * They are distinguishable by their tag field.
 */
struct _BBCC {
    BB*      bb;           /* BB for this cost center */

    Context* cxt;          /* execution context of this BBCC */
    ThreadId tid;          /* only for assertion check purpose */
    UInt     rec_index;    /* Recursion index in rec->bbcc for this bbcc */
    BBCC**   rec_array;    /* Variable sized array of pointers to 
			    * recursion BBCCs. Shared. */
    ULong    ret_counter;  /* how often returned from jccs of this bbcc;
			    * used to check if a dump for this BBCC is needed */
    
    BBCC*    next_bbcc;    /* Chain of BBCCs for same BB */
    BBCC*    lru_next_bbcc; /* BBCC executed next the last time */
    
    jCC*     lru_from_jcc; /* Temporary: Cached for faster access (LRU) */
    jCC*     lru_to_jcc;   /* Temporary: Cached for faster access (LRU) */
    FullCost skipped;      /* cost for skipped functions called from 
			    * jmp_addr. Allocated lazy */
    
    BBCC*    next;         /* entry chain in hash */
    ULong*   cost;         /* start of 64bit costs for this BBCC */
    ULong    ecounter_sum; /* execution counter for first instruction of BB */
    JmpData  jmp[0];
};


/* the <number> of fn_node, file_node and obj_node are for compressed dumping
 * and a index into the dump boolean table and fn_info_table
 */

struct _fn_node {
  HChar*     name;
  UInt       number;
  Context*   last_cxt; /* LRU info */
  Context*   pure_cxt; /* the context with only the function itself */
  file_node* file;     /* reverse mapping for 2nd hash */
  fn_node* next;

  Bool dump_before :1;
  Bool dump_after :1;
  Bool zero_before :1;
  Bool toggle_collect :1;
  Bool skip :1;
  Bool pop_on_jump : 1;

  Bool is_malloc :1;
  Bool is_realloc :1;
  Bool is_free :1;

  Int  group;
  Int  separate_callers;
  Int  separate_recursions;
#if CLG_ENABLE_DEBUG
  Int  verbosity; /* Stores old verbosity level while in function */
#endif
};

/* Quite arbitrary fixed hash sizes */

#define   N_OBJ_ENTRIES         47
#define  N_FILE_ENTRIES         53
#define    N_FN_ENTRIES         87

struct _file_node {
   HChar*     name;
   fn_node*   fns[N_FN_ENTRIES];
   UInt       number;
   obj_node*  obj;
   file_node* next;
};

/* If an object is dlopened multiple times, we hope that <name> is unique;
 * <start> and <offset> can change with each dlopen, and <start> is
 * zero when object is unmapped (possible at dump time).
 */
struct _obj_node {
   const HChar* name;
   UInt       last_slash_pos;

   Addr       start;  /* Start address of text segment mapping */
   SizeT      size;   /* Length of mapping */
   PtrdiffT   offset; /* Offset between symbol address and file offset */

   file_node* files[N_FILE_ENTRIES];
   UInt       number;
   obj_node*  next;
};

/* an entry in the callstack
 *
 * <nonskipped> is 0 if the function called is not skipped (usual case).
 * Otherwise, it is the last non-skipped BBCC. This one gets all
 * the calls to non-skipped functions and all costs in skipped 
 * instructions.
 */
struct _call_entry {
    jCC* jcc;           /* jCC for this call */
    FullCost enter_cost; /* cost event counters at entering frame */
    Addr sp;            /* stack pointer directly after call */
    Addr ret_addr;      /* address to which to return to
			 * is 0 on a simulated call */
    BBCC* nonskipped;   /* see above */
    Context* cxt;       /* context before call */
    Int fn_sp;          /* function stack index before call */
};


/*
 * Execution state of main thread or a running signal handler in
 * a thread while interrupted by another signal handler.
 * As there's no scheduling among running signal handlers of one thread,
 * we only need a subset of a full thread state:
 * - event counter
 * - collect state
 * - last BB, last jump kind, last nonskipped BB
 * - callstack pointer for sanity checking and correct unwinding
 *   after exit
 */
typedef struct _exec_state exec_state;
struct _exec_state {

  /* the signum of the handler, 0 for main thread context
   */
  Int sig;
  
  /* the old call stack pointer at entering the signal handler */
  Int orig_sp;
  
  FullCost cost;
  Bool     collect;
  Context* cxt;
  
  /* number of conditional jumps passed in last BB */
  Int   jmps_passed;
  BBCC* bbcc;      /* last BB executed */
  BBCC* nonskipped;

  Int call_stack_bottom; /* Index into fn_stack */
};

/* Global state structures */
typedef struct _bb_hash bb_hash;
struct _bb_hash {
  UInt size, entries;
  BB** table;
};

typedef struct _cxt_hash cxt_hash;
struct _cxt_hash {
  UInt size, entries;
  Context** table;
};  

/* Thread specific state structures, i.e. parts of a thread state.
 * There are variables for the current state of each part,
 * on which a thread state is copied at thread switch.
 */
typedef struct _bbcc_hash bbcc_hash;
struct _bbcc_hash {
  UInt size, entries;
  BBCC** table;
};

typedef struct _jcc_hash jcc_hash;
struct _jcc_hash {
  UInt size, entries;
  jCC** table;
  jCC* spontaneous;
};

typedef struct _fn_array fn_array;
struct _fn_array {
  UInt size;
  UInt* array;
};

typedef struct _call_stack call_stack;
struct _call_stack {
  UInt size;
  Int sp;
  call_entry* entry;
};

typedef struct _fn_stack fn_stack;
struct _fn_stack {
  UInt size;
  fn_node **bottom, **top;
};

/* The maximum number of simultaneous running signal handlers per thread.
 * This is the number of execution states storable in a thread.
 */
#define MAX_SIGHANDLERS 10

typedef struct _exec_stack exec_stack;
struct _exec_stack {
  Int sp; /* > 0 if a handler is running */
  exec_state* entry[MAX_SIGHANDLERS];
};

/* Thread State 
 *
 * This structure stores thread specific info while a thread is *not*
 * running. See function switch_thread() for save/restore on thread switch.
 *
 * If --separate-threads=no, BBCCs and JCCs can be shared by all threads, i.e.
 * only structures of thread 1 are used.
 * This involves variables fn_info_table, bbcc_table and jcc_table.
 */
struct _thread_info {

  /* state */
  fn_stack fns;       /* function stack */
  call_stack calls;   /* context call arc stack */
  exec_stack states;  /* execution states interrupted by signals */

  /* dump statistics */
  FullCost lastdump_cost;    /* Cost at last dump */
  FullCost sighandler_cost;

  /* thread specific data structure containers */
  fn_array fn_active;
  jcc_hash jccs;
  bbcc_hash bbccs;
};

/* Structs used for dumping */

/* Address position inside of a BBCC:
 * This includes
 * - the address offset from the BB start address
 * - file/line from debug info for that address (can change inside a BB)
 */
typedef struct _AddrPos AddrPos;
struct _AddrPos {
    Addr addr;
    Addr bb_addr;
    file_node* file;
    UInt line;
};

/* a simulator cost entity that can be written out in one line */
typedef struct _AddrCost AddrCost;
struct _AddrCost {
    AddrPos p;
    SimCost cost;
};

/* A function in an execution context */
typedef struct _FnPos FnPos;
struct _FnPos {
    file_node* file;
    fn_node* fn;
    obj_node* obj;
    Context* cxt;
    int rec_index;
    UInt line;
};

/*------------------------------------------------------------*/
/*--- Cache simulator interface                            ---*/
/*------------------------------------------------------------*/

struct cachesim_if
{
    void (*print_opts)(void);
    Bool (*parse_opt)(const HChar* arg);
    void (*post_clo_init)(void);
    void (*clear)(void);
    void (*dump_desc)(VgFile *fp);
    void (*printstat)(Int,Int,Int);
    void (*add_icost)(SimCost, BBCC*, InstrInfo*, ULong);
    void (*finish)(void);
    
    void (*log_1I0D)(InstrInfo*) VG_REGPARM(1);
    void (*log_2I0D)(InstrInfo*, InstrInfo*) VG_REGPARM(2);
    void (*log_3I0D)(InstrInfo*, InstrInfo*, InstrInfo*) VG_REGPARM(3);

    void (*log_1I1Dr)(InstrInfo*, Addr, Word) VG_REGPARM(3);
    void (*log_1I1Dw)(InstrInfo*, Addr, Word) VG_REGPARM(3);

    void (*log_0I1Dr)(InstrInfo*, Addr, Word) VG_REGPARM(3);
    void (*log_0I1Dw)(InstrInfo*, Addr, Word) VG_REGPARM(3);

    // function names of helpers (for debugging generated code)
    const HChar *log_1I0D_name, *log_2I0D_name, *log_3I0D_name;
    const HChar *log_1I1Dr_name, *log_1I1Dw_name;
    const HChar *log_0I1Dr_name, *log_0I1Dw_name;
};

// Event groups
#define EG_USE   0
#define EG_IR    1
#define EG_DR    2
#define EG_DW    3
#define EG_BC    4
#define EG_BI    5
#define EG_BUS   6
#define EG_ALLOC 7
#define EG_SYS   8

struct event_sets {
    EventSet *base, *full;
};

#define fullOffset(group) (CLG_(sets).full->offset[group])


/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

/* from clo.c */

void CLG_(set_clo_defaults)(void);
void CLG_(update_fn_config)(fn_node*);
Bool CLG_(process_cmd_line_option)(const HChar*);
void CLG_(print_usage)(void);
void CLG_(print_debug_usage)(void);

/* from sim.c */
void CLG_(init_eventsets)(void);

/* from main.c */
Bool CLG_(get_debug_info)(Addr, const HChar **dirname,
                          const HChar **filename,
                          const HChar **fn_name, UInt*, DebugInfo**);
void CLG_(collectBlockInfo)(IRSB* bbIn, UInt*, UInt*, Bool*);
void CLG_(set_instrument_state)(const HChar*,Bool);
void CLG_(dump_profile)(const HChar* trigger,Bool only_current_thread);
void CLG_(zero_all_cost)(Bool only_current_thread);
Int CLG_(get_dump_counter)(void);
void CLG_(fini)(Int exitcode);

/* from bb.c */
void CLG_(init_bb_hash)(void);
bb_hash* CLG_(get_bb_hash)(void);
BB*  CLG_(get_bb)(Addr addr, IRSB* bb_in, Bool *seen_before);
void CLG_(delete_bb)(Addr addr);

static __inline__ Addr bb_addr(BB* bb)
 { return bb->offset + bb->obj->offset; }
static __inline__ Addr bb_jmpaddr(BB* bb)
 { UInt off = (bb->instr_count > 0) ? bb->instr[bb->instr_count-1].instr_offset : 0;
   return off + bb->offset + bb->obj->offset; }

/* from fn.c */
void CLG_(init_fn_array)(fn_array*);
void CLG_(copy_current_fn_array)(fn_array* dst);
fn_array* CLG_(get_current_fn_array)(void);
void CLG_(set_current_fn_array)(fn_array*);
UInt* CLG_(get_fn_entry)(Int n);

void      CLG_(init_obj_table)(void);
obj_node* CLG_(get_obj_node)(DebugInfo* si);
file_node* CLG_(get_file_node)(obj_node*, const HChar *dirname,
                               const HChar* filename);
fn_node*  CLG_(get_fn_node)(BB* bb);

/* from bbcc.c */
void CLG_(init_bbcc_hash)(bbcc_hash* bbccs);
void CLG_(copy_current_bbcc_hash)(bbcc_hash* dst);
bbcc_hash* CLG_(get_current_bbcc_hash)(void);
void CLG_(set_current_bbcc_hash)(bbcc_hash*);
void CLG_(forall_bbccs)(void (*func)(BBCC*));
void CLG_(zero_bbcc)(BBCC* bbcc);
BBCC* CLG_(get_bbcc)(BB* bb);
BBCC* CLG_(clone_bbcc)(BBCC* orig, Context* cxt, Int rec_index);
void CLG_(setup_bbcc)(BB* bb) VG_REGPARM(1);


/* from jumps.c */
void CLG_(init_jcc_hash)(jcc_hash*);
void CLG_(copy_current_jcc_hash)(jcc_hash* dst);
void CLG_(set_current_jcc_hash)(jcc_hash*);
jCC* CLG_(get_jcc)(BBCC* from, UInt, BBCC* to);

/* from callstack.c */
void CLG_(init_call_stack)(call_stack*);
void CLG_(copy_current_call_stack)(call_stack* dst);
void CLG_(set_current_call_stack)(call_stack*);
call_entry* CLG_(get_call_entry)(Int n);

void CLG_(push_call_stack)(BBCC* from, UInt jmp, BBCC* to, Addr sp, Bool skip);
void CLG_(pop_call_stack)(void);
Int CLG_(unwind_call_stack)(Addr sp, Int);

/* from context.c */
void CLG_(init_fn_stack)(fn_stack*);
void CLG_(copy_current_fn_stack)(fn_stack*);
void CLG_(set_current_fn_stack)(fn_stack*);

void CLG_(init_cxt_table)(void);
Context* CLG_(get_cxt)(fn_node** fn);
void CLG_(push_cxt)(fn_node* fn);

/* from threads.c */
void CLG_(init_threads)(void);
thread_info** CLG_(get_threads)(void);
thread_info* CLG_(get_current_thread)(void);
void CLG_(switch_thread)(ThreadId tid);
void CLG_(forall_threads)(void (*func)(thread_info*));
void CLG_(run_thread)(ThreadId tid);

void CLG_(init_exec_state)(exec_state* es);
void CLG_(init_exec_stack)(exec_stack*);
void CLG_(copy_current_exec_stack)(exec_stack*);
void CLG_(set_current_exec_stack)(exec_stack*);
void CLG_(pre_signal)(ThreadId tid, Int sigNum, Bool alt_stack);
void CLG_(post_signal)(ThreadId tid, Int sigNum);
void CLG_(run_post_signal_on_call_stack_bottom)(void);

/* from dump.c */
void CLG_(init_dumps)(void);

/*------------------------------------------------------------*/
/*--- Exported global variables                            ---*/
/*------------------------------------------------------------*/

extern CommandLineOptions CLG_(clo);
extern Statistics CLG_(stat);
extern EventMapping* CLG_(dumpmap);

/* Function active counter array, indexed by function number */
extern UInt* CLG_(fn_active_array);
extern Bool CLG_(instrument_state);
 /* min of L1 and LL cache line sizes */
extern Int CLG_(min_line_size);
extern call_stack CLG_(current_call_stack);
extern fn_stack   CLG_(current_fn_stack);
extern exec_state CLG_(current_state);
extern ThreadId   CLG_(current_tid);
extern FullCost   CLG_(total_cost);
extern struct cachesim_if CLG_(cachesim);
extern struct event_sets  CLG_(sets);

// set by setup_bbcc at start of every BB, and needed by log_* helpers
extern Addr   CLG_(bb_base);
extern ULong* CLG_(cost_base);


/*------------------------------------------------------------*/
/*--- Debug output                                         ---*/
/*------------------------------------------------------------*/

#if CLG_ENABLE_DEBUG

#define CLG_DEBUGIF(x) \
  if (UNLIKELY( (CLG_(clo).verbose >x) && \
                (CLG_(stat).bb_executions >= CLG_(clo).verbose_start)))

#define CLG_DEBUG(x,format,args...)   \
    CLG_DEBUGIF(x) {                  \
      CLG_(print_bbno)();	      \
      VG_(printf)(format,##args);     \
    }

#define CLG_ASSERT(cond)              \
    if (UNLIKELY(!(cond))) {          \
      CLG_(print_context)();          \
      CLG_(print_bbno)();	      \
      tl_assert(cond);                \
     }

#else
#define CLG_DEBUGIF(x) if (0)
#define CLG_DEBUG(x...) {}
#define CLG_ASSERT(cond) tl_assert(cond);
#endif

/* from debug.c */
void CLG_(print_bbno)(void);
void CLG_(print_context)(void);
void CLG_(print_jcc)(int s, jCC* jcc);
void CLG_(print_bbcc)(int s, BBCC* bbcc);
void CLG_(print_bbcc_fn)(BBCC* bbcc);
void CLG_(print_execstate)(int s, exec_state* es);
void CLG_(print_eventset)(int s, EventSet* es);
void CLG_(print_cost)(int s, EventSet*, ULong* cost);
void CLG_(print_bb)(int s, BB* bb);
void CLG_(print_bbcc_cost)(int s, BBCC*);
void CLG_(print_cxt)(int s, Context* cxt, int rec_index);
void CLG_(print_short_jcc)(jCC* jcc);
void CLG_(print_stackentry)(int s, int sp);
void CLG_(print_addr)(Addr addr);
void CLG_(print_addr_ln)(Addr addr);

void* CLG_(malloc)(const HChar* cc, UWord s, const HChar* f);
void* CLG_(free)(void* p, const HChar* f);
#if 0
#define CLG_MALLOC(_cc,x) CLG_(malloc)((_cc),x,__FUNCTION__)
#define CLG_FREE(p)       CLG_(free)(p,__FUNCTION__)
#else
#define CLG_MALLOC(_cc,x) VG_(malloc)((_cc),x)
#define CLG_FREE(p)       VG_(free)(p)
#endif

#endif /* CLG_GLOBAL */
