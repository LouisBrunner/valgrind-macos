//--------------------------------------------------------------------*/
//--- Massif: a heap profiling tool.                     ms_main.c ---*/
//--------------------------------------------------------------------*/

/*
   This file is part of Massif, a Valgrind tool for profiling memory
   usage of programs.

   Copyright (C) 2003-2010 Nicholas Nethercote
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

//---------------------------------------------------------------------------
// XXX:
//---------------------------------------------------------------------------
// Todo -- nice, but less critical:
// - do a graph-drawing test
// - make file format more generic.  Obstacles:
//   - unit prefixes are not generic
//   - preset column widths for stats are not generic
//   - preset column headers are not generic
//   - "Massif arguments:" line is not generic
// - do snapshots on client requests
//   - (Michael Meeks): have an interactive way to request a dump
//     (callgrind_control-style)
//     - "profile now"
//     - "show me the extra allocations since the last snapshot"
//     - "start/stop logging" (eg. quickly skip boring bits)
// - Add ability to draw multiple graphs, eg. heap-only, stack-only, total.
//   Give each graph a title.  (try to do it generically!)
// - allow truncation of long fnnames if the exact line number is
//   identified?  [hmm, could make getting the name of alloc-fns more
//   difficult] [could dump full names to file, truncate in ms_print]
// - make --show-below-main=no work
// - Options like --alloc-fn='operator new(unsigned, std::nothrow_t const&)'
//   don't work in a .valgrindrc file or in $VALGRIND_OPTS. 
//   m_commandline.c:add_args_from_string() needs to respect single quotes.
// - With --stack=yes, want to add a stack trace for detailed snapshots so
//   it's clear where/why the peak is occurring. (Mattieu Castet)  Also,
//   possibly useful even with --stack=no? (Andi Yin)
//
// Performance:
// - To run the benchmarks:
//
//     perl perf/vg_perf --tools=massif --reps=3 perf/{heap,tinycc} massif
//     time valgrind --tool=massif --depth=100 konqueror
//
//   The other benchmarks don't do much allocation, and so give similar speeds
//   to Nulgrind.
//
//   Timing results on 'nevermore' (njn's machine) as of r7013:
//
//     heap      0.53s  ma:12.4s (23.5x, -----)
//     tinycc    0.46s  ma: 4.9s (10.7x, -----)
//     many-xpts 0.08s  ma: 2.0s (25.0x, -----)
//     konqueror 29.6s real  0:21.0s user
//
//   [Introduction of --time-unit=i as the default slowed things down by
//   roughly 0--20%.]
//
// - get_XCon accounts for about 9% of konqueror startup time.  Try
//   keeping XPt children sorted by 'ip' and use binary search in get_XCon.
//   Requires factoring out binary search code from various places into a
//   VG_(bsearch) function.  
//
// Todo -- low priority:
// - In each XPt, record both bytes and the number of allocations, and
//   possibly the global number of allocations.
// - (Andy Lin) Give a stack trace on detailed snapshots?
// - (Artur Wisz) add a feature to Massif to ignore any heap blocks larger
//   than a certain size!  Because: "linux's malloc allows to set a
//   MMAP_THRESHOLD value, so we set it to 4096 - all blocks above that will
//   be handled directly by the kernel, and are guaranteed to be returned to
//   the system when freed. So we needed to profile only blocks below this
//   limit."
//
// File format working notes:

#if 0
desc: --heap-admin=foo
cmd: date
time_unit: ms
#-----------
snapshot=0
#-----------
time=0
mem_heap_B=0
mem_heap_admin_B=0
mem_stacks_B=0
heap_tree=empty
#-----------
snapshot=1
#-----------
time=353
mem_heap_B=5
mem_heap_admin_B=0
mem_stacks_B=0
heap_tree=detailed
n1: 5 (heap allocation functions) malloc/new/new[], --alloc-fns, etc.
 n1: 5 0x27F6E0: _nl_normalize_codeset (in /lib/libc-2.3.5.so)
  n1: 5 0x279DE6: _nl_load_locale_from_archive (in /lib/libc-2.3.5.so)
   n1: 5 0x278E97: _nl_find_locale (in /lib/libc-2.3.5.so)
    n1: 5 0x278871: setlocale (in /lib/libc-2.3.5.so)
     n1: 5 0x8049821: (within /bin/date)
      n0: 5 0x26ED5E: (below main) (in /lib/libc-2.3.5.so)


n_events: n  time(ms)  total(B)    useful-heap(B)  admin-heap(B)  stacks(B)
t_events: B
n 0 0 0 0 0 
n 0 0 0 0 0
t1: 5 <string...>
 t1: 6 <string...>

Ideas:
- each snapshot specifies an x-axis value and one or more y-axis values.
- can display the y-axis values separately if you like
- can completely separate connection between snapshots and trees.

Challenges:
- how to specify and scale/abbreviate units on axes?
- how to combine multiple values into the y-axis?

--------------------------------------------------------------------------------Command:            date
Massif arguments:   --heap-admin=foo
ms_print arguments: massif.out
--------------------------------------------------------------------------------
    KB
6.472^                                                       :#
     |                                                       :#  ::  .    .
     ...
     |                                     ::@  :@    :@ :@:::#  ::  :    ::::
   0 +-----------------------------------@---@---@-----@--@---#-------------->ms     0                                                                     713

Number of snapshots: 50
 Detailed snapshots: [2, 11, 13, 19, 25, 32 (peak)]
--------------------------------------------------------------------------------  n       time(ms)         total(B)   useful-heap(B) admin-heap(B)    stacks(B)
--------------------------------------------------------------------------------  0              0                0                0             0            0
  1            345                5                5             0            0
  2            353                5                5             0            0
100.00% (5B) (heap allocation functions) malloc/new/new[], --alloc-fns, etc.
->100.00% (5B) 0x27F6E0: _nl_normalize_codeset (in /lib/libc-2.3.5.so)
#endif

//---------------------------------------------------------------------------

#include "pub_tool_basics.h"
#include "pub_tool_vki.h"
#include "pub_tool_aspacemgr.h"
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
#include "pub_tool_replacemalloc.h"
#include "pub_tool_stacktrace.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"
#include "pub_tool_clientstate.h"

#include "valgrind.h"           // For {MALLOC,FREE}LIKE_BLOCK

//------------------------------------------------------------*/
//--- Overview of operation                                ---*/
//------------------------------------------------------------*/

// The size of the stacks and heap is tracked.  The heap is tracked in a lot
// of detail, enough to tell how many bytes each line of code is responsible
// for, more or less.  The main data structure is a tree representing the
// call tree beneath all the allocation functions like malloc().
// (Alternatively, if --pages-as-heap=yes is specified, memory is tracked at
// the page level, and each page is treated much like a heap block.  We use
// "heap" throughout below to cover this case because the concepts are all the
// same.)
//
// "Snapshots" are recordings of the memory usage.  There are two basic
// kinds:
// - Normal:  these record the current time, total memory size, total heap
//   size, heap admin size and stack size.
// - Detailed: these record those things in a normal snapshot, plus a very
//   detailed XTree (see below) indicating how the heap is structured.
//
// Snapshots are taken every so often.  There are two storage classes of
// snapshots:
// - Temporary:  Massif does a temporary snapshot every so often.  The idea
//   is to always have a certain number of temporary snapshots around.  So
//   we take them frequently to begin with, but decreasingly often as the
//   program continues to run.  Also, we remove some old ones after a while.
//   Overall it's a kind of exponential decay thing.  Most of these are
//   normal snapshots, a small fraction are detailed snapshots.
// - Permanent:  Massif takes a permanent (detailed) snapshot in some
//   circumstances.  They are:
//   - Peak snapshot:  When the memory usage peak is reached, it takes a
//     snapshot.  It keeps this, unless the peak is subsequently exceeded,
//     in which case it will overwrite the peak snapshot.
//   - User-requested snapshots:  These are done in response to client
//     requests.  They are always kept.

// Used for printing things when clo_verbosity > 1.
#define VERB(verb, format, args...) \
   if (VG_(clo_verbosity) > verb) { \
      VG_(dmsg)("Massif: " format, ##args); \
   }

// Used for printing stats when clo_stats == True.
#define STATS(format, args...) \
   if (VG_(clo_stats)) { \
      VG_(dmsg)("Massif: " format, ##args); \
   }

//------------------------------------------------------------//
//--- Statistics                                           ---//
//------------------------------------------------------------//

// Konqueror startup, to give an idea of the numbers involved with a biggish
// program, with default depth:
//
//  depth=3                   depth=40
//  - 310,000 allocations
//  - 300,000 frees
//  -  15,000 XPts            800,000 XPts
//  -   1,800 top-XPts

static UInt n_heap_allocs           = 0;
static UInt n_heap_reallocs         = 0;
static UInt n_heap_frees            = 0;
static UInt n_ignored_heap_allocs   = 0;
static UInt n_ignored_heap_frees    = 0;
static UInt n_ignored_heap_reallocs = 0;
static UInt n_stack_allocs          = 0;
static UInt n_stack_frees           = 0;
static UInt n_xpts                  = 0;
static UInt n_xpt_init_expansions   = 0;
static UInt n_xpt_later_expansions  = 0;
static UInt n_sxpt_allocs           = 0;
static UInt n_sxpt_frees            = 0;
static UInt n_skipped_snapshots     = 0;
static UInt n_real_snapshots        = 0;
static UInt n_detailed_snapshots    = 0;
static UInt n_peak_snapshots        = 0;
static UInt n_cullings              = 0;
static UInt n_XCon_redos            = 0;

//------------------------------------------------------------//
//--- Globals                                              ---//
//------------------------------------------------------------//

// Number of guest instructions executed so far.  Only used with
// --time-unit=i.
static Long guest_instrs_executed = 0;

static SizeT heap_szB       = 0; // Live heap size
static SizeT heap_extra_szB = 0; // Live heap extra size -- slop + admin bytes
static SizeT stacks_szB     = 0; // Live stacks size

// This is the total size from the current peak snapshot, or 0 if no peak
// snapshot has been taken yet.
static SizeT peak_snapshot_total_szB = 0;

// Incremented every time memory is allocated/deallocated, by the
// allocated/deallocated amount;  includes heap, heap-admin and stack
// memory.  An alternative to milliseconds as a unit of program "time".
static ULong total_allocs_deallocs_szB = 0;

// When running with --heap=yes --pages-as-heap=no, we don't start taking
// snapshots until the first basic block is executed, rather than doing it in
// ms_post_clo_init (which is the obvious spot), for two reasons.
// - It lets us ignore stack events prior to that, because they're not
//   really proper ones and just would screw things up.
// - Because there's still some core initialisation to do, and so there
//   would be an artificial time gap between the first and second snapshots.
//
// When running with --heap=yes --pages-as-heap=yes, snapshots start much
// earlier due to new_mem_startup so this isn't relevant.
//
static Bool have_started_executing_code = False;

//------------------------------------------------------------//
//--- Alloc fns                                            ---//
//------------------------------------------------------------//

static XArray* alloc_fns;
static XArray* ignore_fns;

static void init_alloc_fns(void)
{
   // Create the list, and add the default elements.
   alloc_fns = VG_(newXA)(VG_(malloc), "ms.main.iaf.1",
                                       VG_(free), sizeof(Char*));
   #define DO(x)  { Char* s = x; VG_(addToXA)(alloc_fns, &s); }

   // Ordered roughly according to (presumed) frequency.
   // Nb: The C++ "operator new*" ones are overloadable.  We include them
   // always anyway, because even if they're overloaded, it would be a
   // prodigiously stupid overloading that caused them to not allocate
   // memory.
   //
   // XXX: because we don't look at the first stack entry (unless it's a
   // custom allocation) there's not much point to having all these alloc
   // functions here -- they should never appear anywhere (I think?) other
   // than the top stack entry.  The only exceptions are those that in
   // vg_replace_malloc.c are partly or fully implemented in terms of another
   // alloc function: realloc (which uses malloc);  valloc,
   // malloc_zone_valloc, posix_memalign and memalign_common (which use
   // memalign).
   //
   DO("malloc"                                              );
   DO("__builtin_new"                                       );
   DO("operator new(unsigned)"                              );
   DO("operator new(unsigned long)"                         );
   DO("__builtin_vec_new"                                   );
   DO("operator new[](unsigned)"                            );
   DO("operator new[](unsigned long)"                       );
   DO("calloc"                                              );
   DO("realloc"                                             );
   DO("memalign"                                            );
   DO("posix_memalign"                                      );
   DO("valloc"                                              );
   DO("operator new(unsigned, std::nothrow_t const&)"       );
   DO("operator new[](unsigned, std::nothrow_t const&)"     );
   DO("operator new(unsigned long, std::nothrow_t const&)"  );
   DO("operator new[](unsigned long, std::nothrow_t const&)");
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   DO("malloc_common"                                       );
   DO("calloc_common"                                       );
   DO("realloc_common"                                      );
   DO("memalign_common"                                     );
#elif defined(VGO_darwin)
   DO("malloc_zone_malloc"                                  );
   DO("malloc_zone_calloc"                                  );
   DO("malloc_zone_realloc"                                 );
   DO("malloc_zone_memalign"                                );
   DO("malloc_zone_valloc"                                  );
#endif
}

static void init_ignore_fns(void)
{
   // Create the (empty) list.
   ignore_fns = VG_(newXA)(VG_(malloc), "ms.main.iif.1",
                                        VG_(free), sizeof(Char*));
}

// Determines if the named function is a member of the XArray.
static Bool is_member_fn(XArray* fns, Char* fnname)
{
   Char** fn_ptr;
   Int i;
 
   // Nb: It's a linear search through the list, because we're comparing
   // strings rather than pointers to strings.
   // Nb: This gets called a lot.  It was an OSet, but they're quite slow to
   // iterate through so it wasn't a good choice.
   for (i = 0; i < VG_(sizeXA)(fns); i++) {
      fn_ptr = VG_(indexXA)(fns, i);
      if (VG_STREQ(fnname, *fn_ptr))
         return True;
   }
   return False;
}


//------------------------------------------------------------//
//--- Command line args                                    ---//
//------------------------------------------------------------//

#define MAX_DEPTH       200

typedef enum { TimeI, TimeMS, TimeB } TimeUnit;

static Char* TimeUnit_to_string(TimeUnit time_unit)
{
   switch (time_unit) {
   case TimeI:  return "i";
   case TimeMS: return "ms";
   case TimeB:  return "B";
   default:     tl_assert2(0, "TimeUnit_to_string: unrecognised TimeUnit");
   }
}

static Bool   clo_heap            = True;
   // clo_heap_admin is deliberately a word-sized type.  At one point it was
   // a UInt, but this caused problems on 64-bit machines when it was
   // multiplied by a small negative number and then promoted to a
   // word-sized type -- it ended up with a value of 4.2 billion.  Sigh.
static SSizeT clo_heap_admin      = 8;
static Bool   clo_pages_as_heap   = False;
static Bool   clo_stacks          = False;
static Int    clo_depth           = 30;
static double clo_threshold       = 1.0;  // percentage
static double clo_peak_inaccuracy = 1.0;  // percentage
static Int    clo_time_unit       = TimeI;
static Int    clo_detailed_freq   = 10;
static Int    clo_max_snapshots   = 100;
static Char*  clo_massif_out_file = "massif.out.%p";

static XArray* args_for_massif;

static Bool ms_process_cmd_line_option(Char* arg)
{
   Char* tmp_str;

   // Remember the arg for later use.
   VG_(addToXA)(args_for_massif, &arg);

        if VG_BOOL_CLO(arg, "--heap",           clo_heap)   {}
   else if VG_BINT_CLO(arg, "--heap-admin",     clo_heap_admin, 0, 1024) {}

   else if VG_BOOL_CLO(arg, "--stacks",         clo_stacks) {}

   else if VG_BOOL_CLO(arg, "--pages-as-heap",  clo_pages_as_heap) {}

   else if VG_BINT_CLO(arg, "--depth",          clo_depth, 1, MAX_DEPTH) {}

   else if VG_STR_CLO(arg, "--alloc-fn",        tmp_str) {
      VG_(addToXA)(alloc_fns, &tmp_str);
   }
   else if VG_STR_CLO(arg, "--ignore-fn",       tmp_str) {
      VG_(addToXA)(ignore_fns, &tmp_str);
   }

   else if VG_DBL_CLO(arg, "--threshold",  clo_threshold) {
      if (clo_threshold < 0 || clo_threshold > 100) {
         VG_(fmsg_bad_option)(arg,
            "--threshold must be between 0.0 and 100.0\n");
      }
   }

   else if VG_DBL_CLO(arg, "--peak-inaccuracy", clo_peak_inaccuracy) {}

   else if VG_XACT_CLO(arg, "--time-unit=i",    clo_time_unit, TimeI)  {}
   else if VG_XACT_CLO(arg, "--time-unit=ms",   clo_time_unit, TimeMS) {}
   else if VG_XACT_CLO(arg, "--time-unit=B",    clo_time_unit, TimeB)  {}

   else if VG_BINT_CLO(arg, "--detailed-freq",  clo_detailed_freq, 1, 1000000) {}

   else if VG_BINT_CLO(arg, "--max-snapshots",  clo_max_snapshots, 10, 1000) {}

   else if VG_STR_CLO(arg, "--massif-out-file", clo_massif_out_file) {}

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

static void ms_print_usage(void)
{
   VG_(printf)(
"    --heap=no|yes             profile heap blocks [yes]\n"
"    --heap-admin=<size>       average admin bytes per heap block;\n"
"                               ignored if --heap=no [8]\n"
"    --stacks=no|yes           profile stack(s) [no]\n"
"    --pages-as-heap=no|yes    profile memory at the page level [no]\n"
"    --depth=<number>          depth of contexts [30]\n"
"    --alloc-fn=<name>         specify <name> as an alloc function [empty]\n"
"    --ignore-fn=<name>        ignore heap allocations within <name> [empty]\n"
"    --threshold=<m.n>         significance threshold, as a percentage [1.0]\n"
"    --peak-inaccuracy=<m.n>   maximum peak inaccuracy, as a percentage [1.0]\n"
"    --time-unit=i|ms|B        time unit: instructions executed, milliseconds\n"
"                              or heap bytes alloc'd/dealloc'd [i]\n"
"    --detailed-freq=<N>       every Nth snapshot should be detailed [10]\n"
"    --max-snapshots=<N>       maximum number of snapshots recorded [100]\n"
"    --massif-out-file=<file>  output file name [massif.out.%%p]\n"
   );
}

static void ms_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
   );
}


//------------------------------------------------------------//
//--- XPts, XTrees and XCons                               ---//
//------------------------------------------------------------//

// An XPt represents an "execution point", ie. a code address.  Each XPt is
// part of a tree of XPts (an "execution tree", or "XTree").  The details of
// the heap are represented by a single XTree.
//
// The root of the tree is 'alloc_xpt', which represents all allocation
// functions, eg:
// - malloc/calloc/realloc/memalign/new/new[];
// - user-specified allocation functions (using --alloc-fn);
// - custom allocation (MALLOCLIKE) points
// It's a bit of a fake XPt (ie. its 'ip' is zero), and is only used because
// it makes the code simpler.
//
// Any child of 'alloc_xpt' is called a "top-XPt".  The XPts at the bottom
// of an XTree (leaf nodes) are "bottom-XPTs".
//
// Each path from a top-XPt to a bottom-XPt through an XTree gives an
// execution context ("XCon"), ie. a stack trace.  (And sub-paths represent
// stack sub-traces.)  The number of XCons in an XTree is equal to the
// number of bottom-XPTs in that XTree.
//
//      alloc_xpt       XTrees are bi-directional.
//        | ^
//        v |
//     > parent <       Example: if child1() calls parent() and child2()
//    /    |     \      also calls parent(), and parent() calls malloc(),
//   |    / \     |     the XTree will look like this.
//   |   v   v    |
//  child1   child2
//
// (Note that malformed stack traces can lead to difficulties.  See the
// comment at the bottom of get_XCon.)
//
// XTrees and XPts are mirrored by SXTrees and SXPts, where the 'S' is short
// for "saved".  When the XTree is duplicated for a snapshot, we duplicate
// it as an SXTree, which is similar but omits some things it does not need,
// and aggregates up insignificant nodes.  This is important as an SXTree is
// typically much smaller than an XTree.

// XXX: make XPt and SXPt extensible arrays, to avoid having to do two
// allocations per Pt.

typedef struct _XPt XPt;
struct _XPt {
   Addr  ip;              // code address

   // Bottom-XPts: space for the precise context.
   // Other XPts:  space of all the descendent bottom-XPts.
   // Nb: this value goes up and down as the program executes.
   SizeT szB;

   XPt*  parent;           // pointer to parent XPt

   // Children.
   // n_children and max_children are 32-bit integers.  16-bit integers
   // are too small -- a very big program might have more than 65536
   // allocation points (ie. top-XPts) -- Konqueror starting up has 1800.
   UInt  n_children;       // number of children
   UInt  max_children;     // capacity of children array
   XPt** children;         // pointers to children XPts
};

typedef
   enum {
      SigSXPt,
      InsigSXPt
   }
   SXPtTag;

typedef struct _SXPt SXPt;
struct _SXPt {
   SXPtTag tag;
   SizeT szB;              // memory size for the node, be it Sig or Insig
   union {
      // An SXPt representing a single significant code location.  Much like
      // an XPt, minus the fields that aren't necessary.
      struct {
         Addr   ip;
         UInt   n_children;
         SXPt** children;
      } 
      Sig;

      // An SXPt representing one or more code locations, all below the
      // significance threshold.
      struct {
         Int   n_xpts;     // number of aggregated XPts
      } 
      Insig;
   };
};

// Fake XPt representing all allocation functions like malloc().  Acts as
// parent node to all top-XPts.
static XPt* alloc_xpt;

// Cheap allocation for blocks that never need to be freed.  Saves about 10%
// for Konqueror startup with --depth=40.
static void* perm_malloc(SizeT n_bytes)
{
   static Addr hp     = 0;    // current heap pointer
   static Addr hp_lim = 0;    // maximum usable byte in current block

   #define SUPERBLOCK_SIZE  (1 << 20)         // 1 MB

   if (hp + n_bytes > hp_lim) {
      hp = (Addr)VG_(am_shadow_alloc)(SUPERBLOCK_SIZE);
      if (0 == hp)
         VG_(out_of_memory_NORETURN)( "massif:perm_malloc",
                                      SUPERBLOCK_SIZE);
      hp_lim = hp + SUPERBLOCK_SIZE - 1;
   }

   hp += n_bytes;

   return (void*)(hp - n_bytes);
}

static XPt* new_XPt(Addr ip, XPt* parent)
{
   // XPts are never freed, so we can use perm_malloc to allocate them.
   // Note that we cannot use perm_malloc for the 'children' array, because
   // that needs to be resizable.
   XPt* xpt    = perm_malloc(sizeof(XPt));
   xpt->ip     = ip;
   xpt->szB    = 0;
   xpt->parent = parent;

   // We don't initially allocate any space for children.  We let that
   // happen on demand.  Many XPts (ie. all the bottom-XPts) don't have any
   // children anyway.
   xpt->n_children   = 0;
   xpt->max_children = 0;
   xpt->children     = NULL;

   // Update statistics
   n_xpts++;

   return xpt;
}

static void add_child_xpt(XPt* parent, XPt* child)
{
   // Expand 'children' if necessary.
   tl_assert(parent->n_children <= parent->max_children);
   if (parent->n_children == parent->max_children) {
      if (0 == parent->max_children) {
         parent->max_children = 4;
         parent->children = VG_(malloc)( "ms.main.acx.1",
                                         parent->max_children * sizeof(XPt*) );
         n_xpt_init_expansions++;
      } else {
         parent->max_children *= 2;    // Double size
         parent->children = VG_(realloc)( "ms.main.acx.2",
                                          parent->children,
                                          parent->max_children * sizeof(XPt*) );
         n_xpt_later_expansions++;
      }
   }

   // Insert new child XPt in parent's children list.
   parent->children[ parent->n_children++ ] = child;
}

// Reverse comparison for a reverse sort -- biggest to smallest.
static Int SXPt_revcmp_szB(void* n1, void* n2)
{
   SXPt* sxpt1 = *(SXPt**)n1;
   SXPt* sxpt2 = *(SXPt**)n2;
   return ( sxpt1->szB < sxpt2->szB ?  1
          : sxpt1->szB > sxpt2->szB ? -1
          :                            0);
}

//------------------------------------------------------------//
//--- XTree Operations                                     ---//
//------------------------------------------------------------//

// Duplicates an XTree as an SXTree.
static SXPt* dup_XTree(XPt* xpt, SizeT total_szB)
{
   Int  i, n_sig_children, n_insig_children, n_child_sxpts;
   SizeT sig_child_threshold_szB;
   SXPt* sxpt;

   // Number of XPt children  Action for SXPT
   // ------------------      ---------------
   // 0 sig, 0 insig          alloc 0 children
   // N sig, 0 insig          alloc N children, dup all
   // N sig, M insig          alloc N+1, dup first N, aggregate remaining M
   // 0 sig, M insig          alloc 1, aggregate M

   // Work out how big a child must be to be significant.  If the current
   // total_szB is zero, then we set it to 1, which means everything will be
   // judged insignificant -- this is sensible, as there's no point showing
   // any detail for this case.  Unless they used --threshold=0, in which
   // case we show them everything because that's what they asked for.
   //
   // Nb: We do this once now, rather than once per child, because if we do
   // that the cost of all the divisions adds up to something significant.
   if (0 == total_szB && 0 != clo_threshold) {
      sig_child_threshold_szB = 1;
   } else {
      sig_child_threshold_szB = (SizeT)((total_szB * clo_threshold) / 100);
   }

   // How many children are significant?  And do we need an aggregate SXPt?
   n_sig_children = 0;
   for (i = 0; i < xpt->n_children; i++) {
      if (xpt->children[i]->szB >= sig_child_threshold_szB) {
         n_sig_children++;
      }
   }
   n_insig_children = xpt->n_children - n_sig_children;
   n_child_sxpts = n_sig_children + ( n_insig_children > 0 ? 1 : 0 );

   // Duplicate the XPt.
   sxpt                 = VG_(malloc)("ms.main.dX.1", sizeof(SXPt));
   n_sxpt_allocs++;
   sxpt->tag            = SigSXPt;
   sxpt->szB            = xpt->szB;
   sxpt->Sig.ip         = xpt->ip;
   sxpt->Sig.n_children = n_child_sxpts;

   // Create the SXPt's children.
   if (n_child_sxpts > 0) {
      Int j;
      SizeT sig_children_szB = 0, insig_children_szB = 0;
      sxpt->Sig.children = VG_(malloc)("ms.main.dX.2", 
                                       n_child_sxpts * sizeof(SXPt*));

      // Duplicate the significant children.  (Nb: sig_children_szB +
      // insig_children_szB doesn't necessarily equal xpt->szB.)
      j = 0;
      for (i = 0; i < xpt->n_children; i++) {
         if (xpt->children[i]->szB >= sig_child_threshold_szB) {
            sxpt->Sig.children[j++] = dup_XTree(xpt->children[i], total_szB);
            sig_children_szB   += xpt->children[i]->szB;
         } else {
            insig_children_szB += xpt->children[i]->szB;
         }
      }

      // Create the SXPt for the insignificant children, if any, and put it
      // in the last child entry.
      if (n_insig_children > 0) {
         // Nb: We 'n_sxpt_allocs' here because creating an Insig SXPt
         // doesn't involve a call to dup_XTree().
         SXPt* insig_sxpt = VG_(malloc)("ms.main.dX.3", sizeof(SXPt));
         n_sxpt_allocs++;
         insig_sxpt->tag = InsigSXPt;
         insig_sxpt->szB = insig_children_szB;
         insig_sxpt->Insig.n_xpts = n_insig_children;
         sxpt->Sig.children[n_sig_children] = insig_sxpt;
      }
   } else {
      sxpt->Sig.children = NULL;
   }

   return sxpt;
}

static void free_SXTree(SXPt* sxpt)
{
   Int  i;
   tl_assert(sxpt != NULL);

   switch (sxpt->tag) {
    case SigSXPt:
      // Free all children SXPts, then the children array.
      for (i = 0; i < sxpt->Sig.n_children; i++) {
         free_SXTree(sxpt->Sig.children[i]);
         sxpt->Sig.children[i] = NULL;
      }
      VG_(free)(sxpt->Sig.children);  sxpt->Sig.children = NULL;
      break;

    case InsigSXPt:
      break;

    default: tl_assert2(0, "free_SXTree: unknown SXPt tag");
   }
   
   // Free the SXPt itself.
   VG_(free)(sxpt);     sxpt = NULL;
   n_sxpt_frees++;
}

// Sanity checking:  we periodically check the heap XTree with
// ms_expensive_sanity_check.
static void sanity_check_XTree(XPt* xpt, XPt* parent)
{
   tl_assert(xpt != NULL);

   // Check back-pointer.
   tl_assert2(xpt->parent == parent,
      "xpt->parent = %p, parent = %p\n", xpt->parent, parent);

   // Check children counts look sane.
   tl_assert(xpt->n_children <= xpt->max_children);

   // Unfortunately, xpt's size is not necessarily equal to the sum of xpt's
   // children's sizes.  See comment at the bottom of get_XCon.
}

// Sanity checking:  we check SXTrees (which are in snapshots) after
// snapshots are created, before they are deleted, and before they are
// printed.
static void sanity_check_SXTree(SXPt* sxpt)
{
   Int i;

   tl_assert(sxpt != NULL);

   // Check the sum of any children szBs equals the SXPt's szB.  Check the
   // children at the same time.
   switch (sxpt->tag) {
    case SigSXPt: {
      if (sxpt->Sig.n_children > 0) {
         for (i = 0; i < sxpt->Sig.n_children; i++) {
            sanity_check_SXTree(sxpt->Sig.children[i]);
         }
      }
      break;
    }
    case InsigSXPt:
      break;         // do nothing

    default: tl_assert2(0, "sanity_check_SXTree: unknown SXPt tag");
   }
}


//------------------------------------------------------------//
//--- XCon Operations                                      ---//
//------------------------------------------------------------//

// This is the limit on the number of removed alloc-fns that can be in a
// single XCon.
#define MAX_OVERESTIMATE   50
#define MAX_IPS            (MAX_DEPTH + MAX_OVERESTIMATE)

// This is used for various buffers which can hold function names/IP
// description.  Some C++ names can get really long so 1024 isn't big
// enough.
#define BUF_LEN   2048

// Determine if the given IP belongs to a function that should be ignored.
static Bool fn_should_be_ignored(Addr ip)
{
   static Char buf[BUF_LEN];
   return
      ( VG_(get_fnname)(ip, buf, BUF_LEN) && is_member_fn(ignore_fns, buf)
      ? True : False );
}

// Get the stack trace for an XCon, filtering out uninteresting entries:
// alloc-fns and entries above alloc-fns, and entries below main-or-below-main.
//   Eg:       alloc-fn1 / alloc-fn2 / a / b / main / (below main) / c
//   becomes:  a / b / main
// Nb: it's possible to end up with an empty trace, eg. if 'main' is marked
// as an alloc-fn.  This is ok.
static
Int get_IPs( ThreadId tid, Bool exclude_first_entry, Addr ips[])
{
   static Char buf[BUF_LEN];
   Int n_ips, i, n_alloc_fns_removed;
   Int overestimate;
   Bool redo;

   // We ask for a few more IPs than clo_depth suggests we need.  Then we
   // remove every entry that is an alloc-fn.  Depending on the
   // circumstances, we may need to redo it all, asking for more IPs.
   // Details:
   // - If the original stack trace is smaller than asked-for, redo=False
   // - Else if after filtering we have >= clo_depth IPs,      redo=False
   // - Else redo=True
   // In other words, to redo, we'd have to get a stack trace as big as we
   // asked for and remove more than 'overestimate' alloc-fns.

   // Main loop.
   redo = True;      // Assume this to begin with.
   for (overestimate = 3; redo; overestimate += 6) {
      // This should never happen -- would require MAX_OVERESTIMATE
      // alloc-fns to be removed from the stack trace.
      if (overestimate > MAX_OVERESTIMATE)
         VG_(tool_panic)("get_IPs: ips[] too small, inc. MAX_OVERESTIMATE?");

      // Ask for more IPs than clo_depth suggests we need.
      n_ips = VG_(get_StackTrace)( tid, ips, clo_depth + overestimate,
                                   NULL/*array to dump SP values in*/,
                                   NULL/*array to dump FP values in*/,
                                   0/*first_ip_delta*/ );
      tl_assert(n_ips > 0);

      // If the original stack trace is smaller than asked-for, redo=False.
      if (n_ips < clo_depth + overestimate) { redo = False; }

      // Filter out alloc fns.  If requested, we automatically remove the
      // first entry (which presumably will be something like malloc or
      // __builtin_new that we're sure to filter out) without looking at it,
      // because VG_(get_fnname) is expensive.
      n_alloc_fns_removed = ( exclude_first_entry ? 1 : 0 );
      for (i = n_alloc_fns_removed; i < n_ips; i++) {
         if (VG_(get_fnname)(ips[i], buf, BUF_LEN)) {
            if (is_member_fn(alloc_fns, buf)) {
               n_alloc_fns_removed++;
            } else {
               break;
            }
         }
      }
      // Remove the alloc fns by shuffling the rest down over them.
      n_ips -= n_alloc_fns_removed;
      for (i = 0; i < n_ips; i++) {
         ips[i] = ips[i + n_alloc_fns_removed];
      }

      // If after filtering we have >= clo_depth IPs, redo=False
      if (n_ips >= clo_depth) {
         redo = False;
         n_ips = clo_depth;      // Ignore any IPs below --depth.
      }

      if (redo) {
         n_XCon_redos++;
      }
   }
   return n_ips;
}

// Gets an XCon and puts it in the tree.  Returns the XCon's bottom-XPt.
// Unless the allocation should be ignored, in which case we return NULL.
static XPt* get_XCon( ThreadId tid, Bool exclude_first_entry )
{
   static Addr ips[MAX_IPS];
   Int i;
   XPt* xpt = alloc_xpt;

   // After this call, the IPs we want are in ips[0]..ips[n_ips-1].
   Int n_ips = get_IPs(tid, exclude_first_entry, ips);

   // Should we ignore this allocation?  (Nb: n_ips can be zero, eg. if
   // 'main' is marked as an alloc-fn.)
   if (n_ips > 0 && fn_should_be_ignored(ips[0])) {
      return NULL;
   }

   // Now do the search/insertion of the XCon.
   for (i = 0; i < n_ips; i++) {
      Addr ip = ips[i];
      Int ch;
      // Look for IP in xpt's children.
      // Linear search, ugh -- about 10% of time for konqueror startup tried
      // caching last result, only hit about 4% for konqueror.
      // Nb:  this search hits about 98% of the time for konqueror
      for (ch = 0; True; ch++) {
         if (ch == xpt->n_children) {
            // IP not found in the children.
            // Create and add new child XPt, then stop.
            XPt* new_child_xpt = new_XPt(ip, xpt);
            add_child_xpt(xpt, new_child_xpt);
            xpt = new_child_xpt;
            break;

         } else if (ip == xpt->children[ch]->ip) {
            // Found the IP in the children, stop.
            xpt = xpt->children[ch];
            break;
         }
      }
   }

   // [Note: several comments refer to this comment.  Do not delete it
   // without updating them.]
   //
   // A complication... If all stack traces were well-formed, then the
   // returned xpt would always be a bottom-XPt.  As a consequence, an XPt's
   // size would always be equal to the sum of its children's sizes, which
   // is an excellent sanity check.  
   //
   // Unfortunately, stack traces occasionally are malformed, ie. truncated.
   // This allows a stack trace to be a sub-trace of another, eg. a/b/c is a
   // sub-trace of a/b/c/d.  So we can't assume this xpt is a bottom-XPt;
   // nor can we do sanity check an XPt's size against its children's sizes.
   // This is annoying, but must be dealt with.  (Older versions of Massif
   // had this assertion in, and it was reported to fail by real users a
   // couple of times.)  Even more annoyingly, I can't come up with a simple
   // test case that exhibit such a malformed stack trace, so I can't
   // regression test it.  Sigh.
   //
   // However, we can print a warning, so that if it happens (unexpectedly)
   // in existing regression tests we'll know.  Also, it warns users that
   // the output snapshots may not add up the way they might expect.
   //
   //tl_assert(0 == xpt->n_children); // Must be bottom-XPt
   if (0 != xpt->n_children) {
      static Int n_moans = 0;
      if (n_moans < 3) {
         VG_(umsg)(
            "Warning: Malformed stack trace detected.  In Massif's output,\n");
         VG_(umsg)(
            "         the size of an entry's child entries may not sum up\n");
         VG_(umsg)(
            "         to the entry's size as they normally do.\n");
         n_moans++;
         if (3 == n_moans)
            VG_(umsg)(
            "         (And Massif now won't warn about this again.)\n");
      }
   }
   return xpt;
}

// Update 'szB' of every XPt in the XCon, by percolating upwards.
static void update_XCon(XPt* xpt, SSizeT space_delta)
{
   tl_assert(clo_heap);
   tl_assert(NULL != xpt);

   if (0 == space_delta)
      return;

   while (xpt != alloc_xpt) {
      if (space_delta < 0) tl_assert(xpt->szB >= -space_delta);
      xpt->szB += space_delta;
      xpt = xpt->parent;
   }
   if (space_delta < 0) tl_assert(alloc_xpt->szB >= -space_delta);
   alloc_xpt->szB += space_delta;
}


//------------------------------------------------------------//
//--- Snapshots                                            ---//
//------------------------------------------------------------//

// Snapshots are done in a way so that we always have a reasonable number of
// them.  We start by taking them quickly.  Once we hit our limit, we cull
// some (eg. half), and start taking them more slowly.  Once we hit the
// limit again, we again cull and then take them even more slowly, and so
// on.

// Time is measured either in i or ms or bytes, depending on the --time-unit
// option.  It's a Long because it can exceed 32-bits reasonably easily, and
// because we need to allow negative values to represent unset times.
typedef Long Time;

#define UNUSED_SNAPSHOT_TIME  -333  // A conspicuous negative number.

typedef
   enum {
      Normal = 77,
      Peak,
      Unused
   }
   SnapshotKind;

typedef
   struct {
      SnapshotKind kind;
      Time  time;
      SizeT heap_szB;
      SizeT heap_extra_szB;// Heap slop + admin bytes.
      SizeT stacks_szB;
      SXPt* alloc_sxpt;    // Heap XTree root, if a detailed snapshot,
   }                       // otherwise NULL.
   Snapshot;

static UInt      next_snapshot_i = 0;  // Index of where next snapshot will go.
static Snapshot* snapshots;            // Array of snapshots.

static Bool is_snapshot_in_use(Snapshot* snapshot)
{
   if (Unused == snapshot->kind) {
      // If snapshot is unused, check all the fields are unset.
      tl_assert(snapshot->time           == UNUSED_SNAPSHOT_TIME);
      tl_assert(snapshot->heap_extra_szB == 0);
      tl_assert(snapshot->heap_szB       == 0);
      tl_assert(snapshot->stacks_szB     == 0);
      tl_assert(snapshot->alloc_sxpt     == NULL);
      return False;
   } else {
      tl_assert(snapshot->time           != UNUSED_SNAPSHOT_TIME);
      return True;
   }
}

static Bool is_detailed_snapshot(Snapshot* snapshot)
{
   return (snapshot->alloc_sxpt ? True : False);
}

static Bool is_uncullable_snapshot(Snapshot* snapshot)
{
   return &snapshots[0] == snapshot                   // First snapshot
       || &snapshots[next_snapshot_i-1] == snapshot   // Last snapshot
       || snapshot->kind == Peak;                     // Peak snapshot
}

static void sanity_check_snapshot(Snapshot* snapshot)
{
   if (snapshot->alloc_sxpt) {
      sanity_check_SXTree(snapshot->alloc_sxpt);
   }
}

// All the used entries should look used, all the unused ones should be clear.
static void sanity_check_snapshots_array(void)
{
   Int i;
   for (i = 0; i < next_snapshot_i; i++) {
      tl_assert( is_snapshot_in_use( & snapshots[i] ));
   }
   for (    ; i < clo_max_snapshots; i++) {
      tl_assert(!is_snapshot_in_use( & snapshots[i] ));
   }
}

// This zeroes all the fields in the snapshot, but does not free the heap
// XTree if present.  It also does a sanity check unless asked not to;  we
// can't sanity check at startup when clearing the initial snapshots because
// they're full of junk.
static void clear_snapshot(Snapshot* snapshot, Bool do_sanity_check)
{
   if (do_sanity_check) sanity_check_snapshot(snapshot);
   snapshot->kind           = Unused;
   snapshot->time           = UNUSED_SNAPSHOT_TIME;
   snapshot->heap_extra_szB = 0;
   snapshot->heap_szB       = 0;
   snapshot->stacks_szB     = 0;
   snapshot->alloc_sxpt     = NULL;
}

// This zeroes all the fields in the snapshot, and frees the heap XTree if
// present.
static void delete_snapshot(Snapshot* snapshot)
{
   // Nb: if there's an XTree, we free it after calling clear_snapshot,
   // because clear_snapshot does a sanity check which includes checking the
   // XTree.
   SXPt* tmp_sxpt = snapshot->alloc_sxpt;
   clear_snapshot(snapshot, /*do_sanity_check*/True);
   if (tmp_sxpt) {
      free_SXTree(tmp_sxpt);
   }
}

static void VERB_snapshot(Int verbosity, Char* prefix, Int i)
{
   Snapshot* snapshot = &snapshots[i];
   Char* suffix;
   switch (snapshot->kind) {
   case Peak:   suffix = "p";                                            break;
   case Normal: suffix = ( is_detailed_snapshot(snapshot) ? "d" : "." ); break;
   case Unused: suffix = "u";                                            break;
   default:
      tl_assert2(0, "VERB_snapshot: unknown snapshot kind: %d", snapshot->kind);
   }
   VERB(verbosity, "%s S%s%3d (t:%lld, hp:%ld, ex:%ld, st:%ld)\n",
      prefix, suffix, i,
      snapshot->time,
      snapshot->heap_szB,
      snapshot->heap_extra_szB,
      snapshot->stacks_szB
   );
}

// Cull half the snapshots;  we choose those that represent the smallest
// time-spans, because that gives us the most even distribution of snapshots
// over time.  (It's possible to lose interesting spikes, however.)
//
// Algorithm for N snapshots:  We find the snapshot representing the smallest
// timeframe, and remove it.  We repeat this until (N/2) snapshots are gone.
// We have to do this one snapshot at a time, rather than finding the (N/2)
// smallest snapshots in one hit, because when a snapshot is removed, its
// neighbours immediately cover greater timespans.  So it's O(N^2), but N is
// small, and it's not done very often.
//
// Once we're done, we return the new smallest interval between snapshots.
// That becomes our minimum time interval.
static UInt cull_snapshots(void)
{
   Int  i, jp, j, jn, min_timespan_i;
   Int  n_deleted = 0;
   Time min_timespan;

   n_cullings++;

   // Sets j to the index of the first not-yet-removed snapshot at or after i
   #define FIND_SNAPSHOT(i, j) \
      for (j = i; \
           j < clo_max_snapshots && !is_snapshot_in_use(&snapshots[j]); \
           j++) { }

   VERB(2, "Culling...\n");

   // First we remove enough snapshots by clearing them in-place.  Once
   // that's done, we can slide the remaining ones down.
   for (i = 0; i < clo_max_snapshots/2; i++) {
      // Find the snapshot representing the smallest timespan.  The timespan
      // for snapshot n = d(N-1,N)+d(N,N+1), where d(A,B) is the time between
      // snapshot A and B.  We don't consider the first and last snapshots for
      // removal.
      Snapshot* min_snapshot;
      Int min_j;

      // Initial triple: (prev, curr, next) == (jp, j, jn)
      // Initial min_timespan is the first one.
      jp = 0;
      FIND_SNAPSHOT(1,   j);
      FIND_SNAPSHOT(j+1, jn);
      min_timespan = 0x7fffffffffffffffLL;
      min_j        = -1;
      while (jn < clo_max_snapshots) {
         Time timespan = snapshots[jn].time - snapshots[jp].time;
         tl_assert(timespan >= 0);
         // Nb: We never cull the peak snapshot.
         if (Peak != snapshots[j].kind && timespan < min_timespan) {
            min_timespan = timespan;
            min_j        = j;
         }
         // Move on to next triple
         jp = j;
         j  = jn;
         FIND_SNAPSHOT(jn+1, jn);
      }
      // We've found the least important snapshot, now delete it.  First
      // print it if necessary.
      tl_assert(-1 != min_j);    // Check we found a minimum.
      min_snapshot = & snapshots[ min_j ];
      if (VG_(clo_verbosity) > 1) {
         Char buf[64];
         VG_(snprintf)(buf, 64, " %3d (t-span = %lld)", i, min_timespan);
         VERB_snapshot(2, buf, min_j);
      }
      delete_snapshot(min_snapshot);
      n_deleted++;
   }

   // Slide down the remaining snapshots over the removed ones.  First set i
   // to point to the first empty slot, and j to the first full slot after
   // i.  Then slide everything down.
   for (i = 0;  is_snapshot_in_use( &snapshots[i] ); i++) { }
   for (j = i; !is_snapshot_in_use( &snapshots[j] ); j++) { }
   for (  ; j < clo_max_snapshots; j++) {
      if (is_snapshot_in_use( &snapshots[j] )) {
         snapshots[i++] = snapshots[j];
         clear_snapshot(&snapshots[j], /*do_sanity_check*/True);
      }
   }
   next_snapshot_i = i;

   // Check snapshots array looks ok after changes.
   sanity_check_snapshots_array();

   // Find the minimum timespan remaining;  that will be our new minimum
   // time interval.  Note that above we were finding timespans by measuring
   // two intervals around a snapshot that was under consideration for
   // deletion.  Here we only measure single intervals because all the
   // deletions have occurred.
   //
   // But we have to be careful -- some snapshots (eg. snapshot 0, and the
   // peak snapshot) are uncullable.  If two uncullable snapshots end up
   // next to each other, they'll never be culled (assuming the peak doesn't
   // change), and the time gap between them will not change.  However, the
   // time between the remaining cullable snapshots will grow ever larger.
   // This means that the min_timespan found will always be that between the
   // two uncullable snapshots, and it will be much smaller than it should
   // be.  To avoid this problem, when computing the minimum timespan, we
   // ignore any timespans between two uncullable snapshots.
   tl_assert(next_snapshot_i > 1);
   min_timespan = 0x7fffffffffffffffLL;
   min_timespan_i = -1;
   for (i = 1; i < next_snapshot_i; i++) {
      if (is_uncullable_snapshot(&snapshots[i]) &&
          is_uncullable_snapshot(&snapshots[i-1]))
      {
         VERB(2, "(Ignoring interval %d--%d when computing minimum)\n", i-1, i);
      } else {
         Time timespan = snapshots[i].time - snapshots[i-1].time;
         tl_assert(timespan >= 0);
         if (timespan < min_timespan) {
            min_timespan = timespan;
            min_timespan_i = i;
         }
      }
   }
   tl_assert(-1 != min_timespan_i);    // Check we found a minimum.

   // Print remaining snapshots, if necessary.
   if (VG_(clo_verbosity) > 1) {
      VERB(2, "Finished culling (%3d of %3d deleted)\n",
         n_deleted, clo_max_snapshots);
      for (i = 0; i < next_snapshot_i; i++) {
         VERB_snapshot(2, "  post-cull", i);
      }
      VERB(2, "New time interval = %lld (between snapshots %d and %d)\n",
         min_timespan, min_timespan_i-1, min_timespan_i);
   }

   return min_timespan;
}

static Time get_time(void)
{
   // Get current time, in whatever time unit we're using.
   if (clo_time_unit == TimeI) {
      return guest_instrs_executed;
   } else if (clo_time_unit == TimeMS) {
      // Some stuff happens between the millisecond timer being initialised
      // to zero and us taking our first snapshot.  We determine that time
      // gap so we can subtract it from all subsequent times so that our
      // first snapshot is considered to be at t = 0ms.  Unfortunately, a
      // bunch of symbols get read after the first snapshot is taken but
      // before the second one (which is triggered by the first allocation),
      // so when the time-unit is 'ms' we always have a big gap between the
      // first two snapshots.  But at least users won't have to wonder why
      // the first snapshot isn't at t=0.
      static Bool is_first_get_time = True;
      static Time start_time_ms;
      if (is_first_get_time) {
         start_time_ms = VG_(read_millisecond_timer)();
         is_first_get_time = False;
         return 0;
      } else {
         return VG_(read_millisecond_timer)() - start_time_ms;
      }
   } else if (clo_time_unit == TimeB) {
      return total_allocs_deallocs_szB;
   } else {
      tl_assert2(0, "bad --time-unit value");
   }
}

// Take a snapshot, and only that -- decisions on whether to take a
// snapshot, or what kind of snapshot, are made elsewhere.
// Nb: we call the arg "my_time" because "time" shadows a global declaration
// in /usr/include/time.h on Darwin.
static void
take_snapshot(Snapshot* snapshot, SnapshotKind kind, Time my_time,
              Bool is_detailed)
{
   tl_assert(!is_snapshot_in_use(snapshot));
   if (!clo_pages_as_heap) {
      tl_assert(have_started_executing_code);
   }

   // Heap and heap admin.
   if (clo_heap) {
      snapshot->heap_szB = heap_szB;
      if (is_detailed) {
         SizeT total_szB = heap_szB + heap_extra_szB + stacks_szB;
         snapshot->alloc_sxpt = dup_XTree(alloc_xpt, total_szB);
         tl_assert(           alloc_xpt->szB == heap_szB);
         tl_assert(snapshot->alloc_sxpt->szB == heap_szB);
      }
      snapshot->heap_extra_szB = heap_extra_szB;
   }

   // Stack(s).
   if (clo_stacks) {
      snapshot->stacks_szB = stacks_szB;
   }

   // Rest of snapshot.
   snapshot->kind = kind;
   snapshot->time = my_time;
   sanity_check_snapshot(snapshot);

   // Update stats.
   if (Peak == kind) n_peak_snapshots++;
   if (is_detailed)  n_detailed_snapshots++;
   n_real_snapshots++;
}


// Take a snapshot, if it's time, or if we've hit a peak.
static void
maybe_take_snapshot(SnapshotKind kind, Char* what)
{
   // 'min_time_interval' is the minimum time interval between snapshots.
   // If we try to take a snapshot and less than this much time has passed,
   // we don't take it.  It gets larger as the program runs longer.  It's
   // initialised to zero so that we begin by taking snapshots as quickly as
   // possible.
   static Time min_time_interval = 0;
   // Zero allows startup snapshot.
   static Time earliest_possible_time_of_next_snapshot = 0;
   static Int  n_snapshots_since_last_detailed         = 0;
   static Int  n_skipped_snapshots_since_last_snapshot = 0;

   Snapshot* snapshot;
   Bool      is_detailed;
   // Nb: we call this variable "my_time" because "time" shadows a global
   // declaration in /usr/include/time.h on Darwin.
   Time      my_time = get_time();

   switch (kind) {
    case Normal: 
      // Only do a snapshot if it's time.
      if (my_time < earliest_possible_time_of_next_snapshot) {
         n_skipped_snapshots++;
         n_skipped_snapshots_since_last_snapshot++;
         return;
      }
      is_detailed = (clo_detailed_freq-1 == n_snapshots_since_last_detailed);
      break;

    case Peak: {
      // Because we're about to do a deallocation, we're coming down from a
      // local peak.  If it is (a) actually a global peak, and (b) a certain
      // amount bigger than the previous peak, then we take a peak snapshot.
      // By not taking a snapshot for every peak, we save a lot of effort --
      // because many peaks remain peak only for a short time.
      SizeT total_szB = heap_szB + heap_extra_szB + stacks_szB;
      SizeT excess_szB_for_new_peak =
         (SizeT)((peak_snapshot_total_szB * clo_peak_inaccuracy) / 100);
      if (total_szB <= peak_snapshot_total_szB + excess_szB_for_new_peak) {
         return;
      }
      is_detailed = True;
      break;
    }

    default:
      tl_assert2(0, "maybe_take_snapshot: unrecognised snapshot kind");
   }

   // Take the snapshot.
   snapshot = & snapshots[next_snapshot_i];
   take_snapshot(snapshot, kind, my_time, is_detailed);

   // Record if it was detailed.
   if (is_detailed) {
      n_snapshots_since_last_detailed = 0;
   } else {
      n_snapshots_since_last_detailed++;
   }

   // Update peak data, if it's a Peak snapshot.
   if (Peak == kind) {
      Int i, number_of_peaks_snapshots_found = 0;

      // Sanity check the size, then update our recorded peak.
      SizeT snapshot_total_szB =
         snapshot->heap_szB + snapshot->heap_extra_szB + snapshot->stacks_szB;
      tl_assert2(snapshot_total_szB > peak_snapshot_total_szB,
         "%ld, %ld\n", snapshot_total_szB, peak_snapshot_total_szB);
      peak_snapshot_total_szB = snapshot_total_szB;

      // Find the old peak snapshot, if it exists, and mark it as normal.
      for (i = 0; i < next_snapshot_i; i++) {
         if (Peak == snapshots[i].kind) {
            snapshots[i].kind = Normal;
            number_of_peaks_snapshots_found++;
         }
      }
      tl_assert(number_of_peaks_snapshots_found <= 1);
   }

   // Finish up verbosity and stats stuff.
   if (n_skipped_snapshots_since_last_snapshot > 0) {
      VERB(2, "  (skipped %d snapshot%s)\n",
         n_skipped_snapshots_since_last_snapshot,
         ( 1 == n_skipped_snapshots_since_last_snapshot ? "" : "s") );
   }
   VERB_snapshot(2, what, next_snapshot_i);
   n_skipped_snapshots_since_last_snapshot = 0;

   // Cull the entries, if our snapshot table is full.
   next_snapshot_i++;
   if (clo_max_snapshots == next_snapshot_i) {
      min_time_interval = cull_snapshots();
   }

   // Work out the earliest time when the next snapshot can happen.
   earliest_possible_time_of_next_snapshot = my_time + min_time_interval;
}


//------------------------------------------------------------//
//--- Sanity checking                                      ---//
//------------------------------------------------------------//

static Bool ms_cheap_sanity_check ( void )
{
   return True;   // Nothing useful we can cheaply check.
}

static Bool ms_expensive_sanity_check ( void )
{
   sanity_check_XTree(alloc_xpt, /*parent*/NULL);
   sanity_check_snapshots_array();
   return True;
}


//------------------------------------------------------------//
//--- Heap management                                      ---//
//------------------------------------------------------------//

// Metadata for heap blocks.  Each one contains a pointer to a bottom-XPt,
// which is a foothold into the XCon at which it was allocated.  From
// HP_Chunks, XPt 'space' fields are incremented (at allocation) and
// decremented (at deallocation).
//
// Nb: first two fields must match core's VgHashNode.
typedef
   struct _HP_Chunk {
      struct _HP_Chunk* next;
      Addr              data;       // Ptr to actual block
      SizeT             req_szB;    // Size requested
      SizeT             slop_szB;   // Extra bytes given above those requested
      XPt*              where;      // Where allocated; bottom-XPt
   }
   HP_Chunk;

static VgHashTable malloc_list  = NULL;   // HP_Chunks

static void update_alloc_stats(SSizeT szB_delta)
{
   // Update total_allocs_deallocs_szB.
   if (szB_delta < 0) szB_delta = -szB_delta;
   total_allocs_deallocs_szB += szB_delta;
}

static void update_heap_stats(SSizeT heap_szB_delta, Int heap_extra_szB_delta)
{
   if (heap_szB_delta < 0)
      tl_assert(heap_szB >= -heap_szB_delta);
   if (heap_extra_szB_delta < 0)
      tl_assert(heap_extra_szB >= -heap_extra_szB_delta);

   heap_extra_szB += heap_extra_szB_delta;
   heap_szB       += heap_szB_delta;

   update_alloc_stats(heap_szB_delta + heap_extra_szB_delta);
}

static
void* record_block( ThreadId tid, void* p, SizeT req_szB, SizeT slop_szB,
                    Bool exclude_first_entry, Bool maybe_snapshot )
{
   // Make new HP_Chunk node, add to malloc_list
   HP_Chunk* hc = VG_(malloc)("ms.main.rb.1", sizeof(HP_Chunk));
   hc->req_szB  = req_szB;
   hc->slop_szB = slop_szB;
   hc->data     = (Addr)p;
   hc->where    = NULL;
   VG_(HT_add_node)(malloc_list, hc);

   if (clo_heap) {
      VERB(3, "<<< record_block (%lu, %lu)\n", req_szB, slop_szB);

      hc->where = get_XCon( tid, exclude_first_entry );

      if (hc->where) {
         // Update statistics.
         n_heap_allocs++;

         // Update heap stats.
         update_heap_stats(req_szB, clo_heap_admin + slop_szB);

         // Update XTree.
         update_XCon(hc->where, req_szB);

         // Maybe take a snapshot.
         if (maybe_snapshot) {
            maybe_take_snapshot(Normal, "  alloc");
         }

      } else {
         // Ignored allocation.
         n_ignored_heap_allocs++;

         VERB(3, "(ignored)\n");
      }

      VERB(3, ">>>\n");
   }

   return p;
}

static __inline__
void* alloc_and_record_block ( ThreadId tid, SizeT req_szB, SizeT req_alignB,
                               Bool is_zeroed )
{
   SizeT actual_szB, slop_szB;
   void* p;

   if ((SSizeT)req_szB < 0) return NULL;

   // Allocate and zero if necessary.
   p = VG_(cli_malloc)( req_alignB, req_szB );
   if (!p) {
      return NULL;
   }
   if (is_zeroed) VG_(memset)(p, 0, req_szB);
   actual_szB = VG_(malloc_usable_size)(p);
   tl_assert(actual_szB >= req_szB);
   slop_szB = actual_szB - req_szB;

   // Record block.
   record_block(tid, p, req_szB, slop_szB, /*exclude_first_entry*/True,
                /*maybe_snapshot*/True);

   return p;
}

static __inline__
void unrecord_block ( void* p, Bool maybe_snapshot )
{
   // Remove HP_Chunk from malloc_list
   HP_Chunk* hc = VG_(HT_remove)(malloc_list, (UWord)p);
   if (NULL == hc) {
      return;   // must have been a bogus free()
   }

   if (clo_heap) {
      VERB(3, "<<< unrecord_block\n");

      if (hc->where) {
         // Update statistics.
         n_heap_frees++;

         // Maybe take a peak snapshot, since it's a deallocation.
         if (maybe_snapshot) {
            maybe_take_snapshot(Peak, "de-PEAK");
         }

         // Update heap stats.
         update_heap_stats(-hc->req_szB, -clo_heap_admin - hc->slop_szB);

         // Update XTree.
         update_XCon(hc->where, -hc->req_szB);

         // Maybe take a snapshot.
         if (maybe_snapshot) {
            maybe_take_snapshot(Normal, "dealloc");
         }

      } else {
         n_ignored_heap_frees++;

         VERB(3, "(ignored)\n");
      }

      VERB(3, ">>> (-%lu, -%lu)\n", hc->req_szB, hc->slop_szB);
   }

   // Actually free the chunk, and the heap block (if necessary)
   VG_(free)( hc );  hc = NULL;
}

// Nb: --ignore-fn is tricky for realloc.  If the block's original alloc was
// ignored, but the realloc is not requested to be ignored, and we are
// shrinking the block, then we have to ignore the realloc -- otherwise we
// could end up with negative heap sizes.  This isn't a danger if we are
// growing such a block, but for consistency (it also simplifies things) we
// ignore such reallocs as well.
static __inline__
void* realloc_block ( ThreadId tid, void* p_old, SizeT new_req_szB )
{
   HP_Chunk* hc;
   void*     p_new;
   SizeT     old_req_szB, old_slop_szB, new_slop_szB, new_actual_szB;
   XPt      *old_where, *new_where;
   Bool      is_ignored = False;

   // Remove the old block
   hc = VG_(HT_remove)(malloc_list, (UWord)p_old);
   if (hc == NULL) {
      return NULL;   // must have been a bogus realloc()
   }

   old_req_szB  = hc->req_szB;
   old_slop_szB = hc->slop_szB;

   tl_assert(!clo_pages_as_heap);  // Shouldn't be here if --pages-as-heap=yes.
   if (clo_heap) {
      VERB(3, "<<< realloc_block (%lu)\n", new_req_szB);

      if (hc->where) {
         // Update statistics.
         n_heap_reallocs++;

         // Maybe take a peak snapshot, if it's (effectively) a deallocation.
         if (new_req_szB < old_req_szB) {
            maybe_take_snapshot(Peak, "re-PEAK");
         }
      } else {
         // The original malloc was ignored, so we have to ignore the
         // realloc as well.
         is_ignored = True;
      }
   }

   // Actually do the allocation, if necessary.
   if (new_req_szB <= old_req_szB + old_slop_szB) {
      // New size is smaller or same;  block not moved.
      p_new = p_old;
      new_slop_szB = old_slop_szB + (old_req_szB - new_req_szB);

   } else {
      // New size is bigger;  make new block, copy shared contents, free old.
      p_new = VG_(cli_malloc)(VG_(clo_alignment), new_req_szB);
      if (!p_new) {
         // Nb: if realloc fails, NULL is returned but the old block is not
         // touched.  What an awful function.
         return NULL;
      }
      VG_(memcpy)(p_new, p_old, old_req_szB);
      VG_(cli_free)(p_old);
      new_actual_szB = VG_(malloc_usable_size)(p_new);
      tl_assert(new_actual_szB >= new_req_szB);
      new_slop_szB = new_actual_szB - new_req_szB;
   }

   if (p_new) {
      // Update HP_Chunk.
      hc->data     = (Addr)p_new;
      hc->req_szB  = new_req_szB;
      hc->slop_szB = new_slop_szB;
      old_where    = hc->where;
      hc->where    = NULL;

      // Update XTree.
      if (clo_heap) {
         new_where = get_XCon( tid, /*exclude_first_entry*/True);
         if (!is_ignored && new_where) {
            hc->where = new_where;
            update_XCon(old_where, -old_req_szB);
            update_XCon(new_where,  new_req_szB);
         } else {
            // The realloc itself is ignored.
            is_ignored = True;

            // Update statistics.
            n_ignored_heap_reallocs++;
         }
      }
   }

   // Now insert the new hc (with a possibly new 'data' field) into
   // malloc_list.  If this realloc() did not increase the memory size, we
   // will have removed and then re-added hc unnecessarily.  But that's ok
   // because shrinking a block with realloc() is (presumably) much rarer
   // than growing it, and this way simplifies the growing case.
   VG_(HT_add_node)(malloc_list, hc);

   if (clo_heap) {
      if (!is_ignored) {
         // Update heap stats.
         update_heap_stats(new_req_szB - old_req_szB,
                          new_slop_szB - old_slop_szB);

         // Maybe take a snapshot.
         maybe_take_snapshot(Normal, "realloc");
      } else {

         VERB(3, "(ignored)\n");
      }

      VERB(3, ">>> (%ld, %ld)\n",
         new_req_szB - old_req_szB, new_slop_szB - old_slop_szB);
   }

   return p_new;
}


//------------------------------------------------------------//
//--- malloc() et al replacement wrappers                  ---//
//------------------------------------------------------------//

static void* ms_malloc ( ThreadId tid, SizeT szB )
{
   return alloc_and_record_block( tid, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* ms___builtin_new ( ThreadId tid, SizeT szB )
{
   return alloc_and_record_block( tid, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* ms___builtin_vec_new ( ThreadId tid, SizeT szB )
{
   return alloc_and_record_block( tid, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* ms_calloc ( ThreadId tid, SizeT m, SizeT szB )
{
   return alloc_and_record_block( tid, m*szB, VG_(clo_alignment), /*is_zeroed*/True );
}

static void *ms_memalign ( ThreadId tid, SizeT alignB, SizeT szB )
{
   return alloc_and_record_block( tid, szB, alignB, False );
}

static void ms_free ( ThreadId tid __attribute__((unused)), void* p )
{
   unrecord_block(p, /*maybe_snapshot*/True);
   VG_(cli_free)(p);
}

static void ms___builtin_delete ( ThreadId tid, void* p )
{
   unrecord_block(p, /*maybe_snapshot*/True);
   VG_(cli_free)(p);
}

static void ms___builtin_vec_delete ( ThreadId tid, void* p )
{
   unrecord_block(p, /*maybe_snapshot*/True);
   VG_(cli_free)(p);
}

static void* ms_realloc ( ThreadId tid, void* p_old, SizeT new_szB )
{
   return realloc_block(tid, p_old, new_szB);
}

static SizeT ms_malloc_usable_size ( ThreadId tid, void* p )
{                                                            
   HP_Chunk* hc = VG_(HT_lookup)( malloc_list, (UWord)p );

   return ( hc ? hc->req_szB + hc->slop_szB : 0 );
}                                                            

//------------------------------------------------------------//
//--- Page handling                                        ---//
//------------------------------------------------------------//

static
void ms_record_page_mem ( Addr a, SizeT len )
{
   ThreadId tid = VG_(get_running_tid)();
   Addr end;
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   tl_assert(len >= VKI_PAGE_SIZE);
   // Record the first N-1 pages as blocks, but don't do any snapshots.
   for (end = a + len - VKI_PAGE_SIZE; a < end; a += VKI_PAGE_SIZE) {
      record_block( tid, (void*)a, VKI_PAGE_SIZE, /*slop_szB*/0,
                    /*exclude_first_entry*/False, /*maybe_snapshot*/False );
   }
   // Record the last page as a block, and maybe do a snapshot afterwards.
   record_block( tid, (void*)a, VKI_PAGE_SIZE, /*slop_szB*/0,
                 /*exclude_first_entry*/False, /*maybe_snapshot*/True );
}

static
void ms_unrecord_page_mem( Addr a, SizeT len )
{
   Addr end;
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   tl_assert(len >= VKI_PAGE_SIZE);
   for (end = a + len - VKI_PAGE_SIZE; a < end; a += VKI_PAGE_SIZE) {
      unrecord_block((void*)a, /*maybe_snapshot*/False);
   }
   unrecord_block((void*)a, /*maybe_snapshot*/True);
}

//------------------------------------------------------------//

static
void ms_new_mem_mmap ( Addr a, SizeT len,
                       Bool rr, Bool ww, Bool xx, ULong di_handle )
{
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   ms_record_page_mem(a, len);
}

static
void ms_new_mem_startup( Addr a, SizeT len,
                         Bool rr, Bool ww, Bool xx, ULong di_handle )
{
   // startup maps are always be page-sized, except the trampoline page is
   // marked by the core as only being the size of the trampoline itself,
   // which is something like 57 bytes.  Round it up to page size.
   len = VG_PGROUNDUP(len);
   ms_record_page_mem(a, len);
}

static
void ms_new_mem_brk ( Addr a, SizeT len, ThreadId tid )
{
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   ms_record_page_mem(a, len);
}

static
void ms_copy_mem_remap( Addr from, Addr to, SizeT len)
{
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   ms_unrecord_page_mem(from, len);
   ms_record_page_mem(to, len);
}

static
void ms_die_mem_munmap( Addr a, SizeT len )
{
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   ms_unrecord_page_mem(a, len);
}

static
void ms_die_mem_brk( Addr a, SizeT len )
{
   tl_assert(VG_IS_PAGE_ALIGNED(len));
   ms_unrecord_page_mem(a, len);
}

//------------------------------------------------------------//
//--- Stacks                                               ---//
//------------------------------------------------------------//

// We really want the inlining to occur...
#define INLINE    inline __attribute__((always_inline))

static void update_stack_stats(SSizeT stack_szB_delta)
{
   if (stack_szB_delta < 0) tl_assert(stacks_szB >= -stack_szB_delta);
   stacks_szB += stack_szB_delta;

   update_alloc_stats(stack_szB_delta);
}

static INLINE void new_mem_stack_2(SizeT len, Char* what)
{
   if (have_started_executing_code) {
      VERB(3, "<<< new_mem_stack (%ld)\n", len);
      n_stack_allocs++;
      update_stack_stats(len);
      maybe_take_snapshot(Normal, what);
      VERB(3, ">>>\n");
   }
}

static INLINE void die_mem_stack_2(SizeT len, Char* what)
{
   if (have_started_executing_code) {
      VERB(3, "<<< die_mem_stack (%ld)\n", -len);
      n_stack_frees++;
      maybe_take_snapshot(Peak,   "stkPEAK");
      update_stack_stats(-len);
      maybe_take_snapshot(Normal, what);
      VERB(3, ">>>\n");
   }
}

static void new_mem_stack(Addr a, SizeT len)
{
   new_mem_stack_2(len, "stk-new");
}

static void die_mem_stack(Addr a, SizeT len)
{
   die_mem_stack_2(len, "stk-die");
}

static void new_mem_stack_signal(Addr a, SizeT len, ThreadId tid)
{
   new_mem_stack_2(len, "sig-new");
}

static void die_mem_stack_signal(Addr a, SizeT len)
{
   die_mem_stack_2(len, "sig-die");
}


//------------------------------------------------------------//
//--- Client Requests                                      ---//
//------------------------------------------------------------//

static Bool ms_handle_client_request ( ThreadId tid, UWord* argv, UWord* ret )
{
   switch (argv[0]) {
   case VG_USERREQ__MALLOCLIKE_BLOCK: {
      void* p   = (void*)argv[1];
      SizeT szB =        argv[2];
      record_block( tid, p, szB, /*slop_szB*/0, /*exclude_first_entry*/False,
                    /*maybe_snapshot*/True );
      *ret = 0;
      return True;
   }
   case VG_USERREQ__FREELIKE_BLOCK: {
      void* p = (void*)argv[1];
      unrecord_block(p, /*maybe_snapshot*/True);
      *ret = 0;
      return True;
   }
   default:
      *ret = 0;
      return False;
   }
}

//------------------------------------------------------------//
//--- Instrumentation                                      ---//
//------------------------------------------------------------//

static void add_counter_update(IRSB* sbOut, Int n)
{
   #if defined(VG_BIGENDIAN)
   # define END Iend_BE
   #elif defined(VG_LITTLEENDIAN)
   # define END Iend_LE
   #else
   # error "Unknown endianness"
   #endif
   // Add code to increment 'guest_instrs_executed' by 'n', like this:
   //   WrTmp(t1, Load64(&guest_instrs_executed))
   //   WrTmp(t2, Add64(RdTmp(t1), Const(n)))
   //   Store(&guest_instrs_executed, t2)
   IRTemp t1 = newIRTemp(sbOut->tyenv, Ity_I64);
   IRTemp t2 = newIRTemp(sbOut->tyenv, Ity_I64);
   IRExpr* counter_addr = mkIRExpr_HWord( (HWord)&guest_instrs_executed );

   IRStmt* st1 = IRStmt_WrTmp(t1, IRExpr_Load(END, Ity_I64, counter_addr));
   IRStmt* st2 =
      IRStmt_WrTmp(t2,
                   IRExpr_Binop(Iop_Add64, IRExpr_RdTmp(t1),
                                           IRExpr_Const(IRConst_U64(n))));
   IRStmt* st3 = IRStmt_Store(END, counter_addr, IRExpr_RdTmp(t2));

   addStmtToIRSB( sbOut, st1 );
   addStmtToIRSB( sbOut, st2 );
   addStmtToIRSB( sbOut, st3 );
}

static IRSB* ms_instrument2( IRSB* sbIn )
{
   Int   i, n = 0;
   IRSB* sbOut;

   // We increment the instruction count in two places:
   // - just before any Ist_Exit statements;
   // - just before the IRSB's end.
   // In the former case, we zero 'n' and then continue instrumenting.
   
   sbOut = deepCopyIRSBExceptStmts(sbIn);
   
   for (i = 0; i < sbIn->stmts_used; i++) {
      IRStmt* st = sbIn->stmts[i];
      
      if (!st || st->tag == Ist_NoOp) continue;
      
      if (st->tag == Ist_IMark) {
         n++;
      } else if (st->tag == Ist_Exit) {
         if (n > 0) {
            // Add an increment before the Exit statement, then reset 'n'.
            add_counter_update(sbOut, n);
            n = 0;
         }
      }
      addStmtToIRSB( sbOut, st );
   }

   if (n > 0) {
      // Add an increment before the SB end.
      add_counter_update(sbOut, n);
   }
   return sbOut;
}

static
IRSB* ms_instrument ( VgCallbackClosure* closure,
                      IRSB* sbIn,
                      VexGuestLayout* layout,
                      VexGuestExtents* vge,
                      IRType gWordTy, IRType hWordTy )
{
   if (! have_started_executing_code) {
      // Do an initial sample to guarantee that we have at least one.
      // We use 'maybe_take_snapshot' instead of 'take_snapshot' to ensure
      // 'maybe_take_snapshot's internal static variables are initialised.
      have_started_executing_code = True;
      maybe_take_snapshot(Normal, "startup");
   }

   if      (clo_time_unit == TimeI)  { return ms_instrument2(sbIn); }
   else if (clo_time_unit == TimeMS) { return sbIn; }
   else if (clo_time_unit == TimeB)  { return sbIn; }
   else                              { tl_assert2(0, "bad --time-unit value"); }
}


//------------------------------------------------------------//
//--- Writing snapshots                                    ---//
//------------------------------------------------------------//

Char FP_buf[BUF_LEN];

// XXX: implement f{,n}printf in m_libcprint.c eventually, and use it here.
// Then change Cachegrind to use it too.
#define FP(format, args...) ({ \
   VG_(snprintf)(FP_buf, BUF_LEN, format, ##args); \
   FP_buf[BUF_LEN-1] = '\0';  /* Make sure the string is terminated. */ \
   VG_(write)(fd, (void*)FP_buf, VG_(strlen)(FP_buf)); \
})

// Nb: uses a static buffer, each call trashes the last string returned.
static Char* make_perc(double x)
{
   static Char mbuf[32];

   VG_(percentify)((ULong)(x * 100), 10000, 2, 6, mbuf);
   // XXX: this is bogus if the denominator was zero -- resulting string is
   // something like "0 --%")
   if (' ' == mbuf[0]) mbuf[0] = '0';
   return mbuf;
}

static void pp_snapshot_SXPt(Int fd, SXPt* sxpt, Int depth, Char* depth_str,
                            Int depth_str_len,
                            SizeT snapshot_heap_szB, SizeT snapshot_total_szB)
{
   Int   i, j, n_insig_children_sxpts;
   SXPt* child = NULL;

   // Used for printing function names.  Is made static to keep it out
   // of the stack frame -- this function is recursive.  Obviously this
   // now means its contents are trashed across the recursive call.
   static Char ip_desc_array[BUF_LEN];
   Char* ip_desc = ip_desc_array;

   switch (sxpt->tag) {
    case SigSXPt:
      // Print the SXPt itself.
      if (0 == depth) {
         if (clo_heap) {
            ip_desc = 
               ( clo_pages_as_heap
               ? "(page allocation syscalls) mmap/mremap/brk, --alloc-fns, etc."
               : "(heap allocation functions) malloc/new/new[], --alloc-fns, etc."
               );
         } else {
            // XXX: --alloc-fns?
         }
      } else {
         // If it's main-or-below-main, we (if appropriate) ignore everything
         // below it by pretending it has no children.
         if ( ! VG_(clo_show_below_main) ) {
            Vg_FnNameKind kind = VG_(get_fnname_kind_from_IP)(sxpt->Sig.ip);
            if (Vg_FnNameMain == kind || Vg_FnNameBelowMain == kind) {
               sxpt->Sig.n_children = 0;
            }
         }

         // We need the -1 to get the line number right, But I'm not sure why.
         ip_desc = VG_(describe_IP)(sxpt->Sig.ip-1, ip_desc, BUF_LEN);
      }
      
      // Do the non-ip_desc part first...
      FP("%sn%d: %lu ", depth_str, sxpt->Sig.n_children, sxpt->szB);

      // For ip_descs beginning with "0xABCD...:" addresses, we first
      // measure the length of the "0xabcd: " address at the start of the
      // ip_desc.
      j = 0;
      if ('0' == ip_desc[0] && 'x' == ip_desc[1]) {
         j = 2;
         while (True) {
            if (ip_desc[j]) {
               if (':' == ip_desc[j]) break;
               j++;
            } else {
               tl_assert2(0, "ip_desc has unexpected form: %s\n", ip_desc);
            }
         }
      }
      // Nb: We treat this specially (ie. we don't use FP) so that if the
      // ip_desc is too long (eg. due to a long C++ function name), it'll
      // get truncated, but the '\n' is still there so its a valid file.
      // (At one point we were truncating without adding the '\n', which
      // caused bug #155929.)
      //
      // Also, we account for the length of the address in ip_desc when
      // truncating.  (The longest address we could have is 18 chars:  "0x"
      // plus 16 address digits.)  This ensures that the truncated function
      // name always has the same length, which makes truncation
      // deterministic and thus makes testing easier.
      tl_assert(j <= 18);
      VG_(snprintf)(FP_buf, BUF_LEN, "%s\n", ip_desc);
      FP_buf[BUF_LEN-18+j-5] = '.';    // "..." at the end make the
      FP_buf[BUF_LEN-18+j-4] = '.';    //   truncation more obvious.
      FP_buf[BUF_LEN-18+j-3] = '.';
      FP_buf[BUF_LEN-18+j-2] = '\n';   // The last char is '\n'.
      FP_buf[BUF_LEN-18+j-1] = '\0';   // The string is terminated.
      VG_(write)(fd, (void*)FP_buf, VG_(strlen)(FP_buf));

      // Indent.
      tl_assert(depth+1 < depth_str_len-1);    // -1 for end NUL char
      depth_str[depth+0] = ' ';
      depth_str[depth+1] = '\0';

      // Sort SXPt's children by szB (reverse order:  biggest to smallest).
      // Nb: we sort them here, rather than earlier (eg. in dup_XTree), for
      // two reasons.  First, if we do it during dup_XTree, it can get
      // expensive (eg. 15% of execution time for konqueror
      // startup/shutdown).  Second, this way we get the Insig SXPt (if one
      // is present) in its sorted position, not at the end.
      VG_(ssort)(sxpt->Sig.children, sxpt->Sig.n_children, sizeof(SXPt*),
                 SXPt_revcmp_szB);

      // Print the SXPt's children.  They should already be in sorted order.
      n_insig_children_sxpts = 0;
      for (i = 0; i < sxpt->Sig.n_children; i++) {
         child = sxpt->Sig.children[i];

         if (InsigSXPt == child->tag)
            n_insig_children_sxpts++;

         // Ok, print the child.  NB: contents of ip_desc_array will be
         // trashed by this recursive call.  Doesn't matter currently,
         // but worth noting.
         pp_snapshot_SXPt(fd, child, depth+1, depth_str, depth_str_len,
            snapshot_heap_szB, snapshot_total_szB);
      }

      // Unindent.
      depth_str[depth+0] = '\0';
      depth_str[depth+1] = '\0';

      // There should be 0 or 1 Insig children SXPts.
      tl_assert(n_insig_children_sxpts <= 1);
      break;

    case InsigSXPt: {
      Char* s = ( 1 == sxpt->Insig.n_xpts ? "," : "s, all" );
      FP("%sn0: %lu in %d place%s below massif's threshold (%s)\n",
         depth_str, sxpt->szB, sxpt->Insig.n_xpts, s,
         make_perc(clo_threshold));
      break;
    }

    default:
      tl_assert2(0, "pp_snapshot_SXPt: unrecognised SXPt tag");
   }
}

static void pp_snapshot(Int fd, Snapshot* snapshot, Int snapshot_n)
{
   sanity_check_snapshot(snapshot);

   FP("#-----------\n");
   FP("snapshot=%d\n", snapshot_n);
   FP("#-----------\n");
   FP("time=%lld\n",            snapshot->time);
   FP("mem_heap_B=%lu\n",       snapshot->heap_szB);
   FP("mem_heap_extra_B=%lu\n", snapshot->heap_extra_szB);
   FP("mem_stacks_B=%lu\n",     snapshot->stacks_szB);

   if (is_detailed_snapshot(snapshot)) {
      // Detailed snapshot -- print heap tree.
      Int   depth_str_len = clo_depth + 3;
      Char* depth_str = VG_(malloc)("ms.main.pps.1", 
                                    sizeof(Char) * depth_str_len);
      SizeT snapshot_total_szB =
         snapshot->heap_szB + snapshot->heap_extra_szB + snapshot->stacks_szB;
      depth_str[0] = '\0';   // Initialise depth_str to "".

      FP("heap_tree=%s\n", ( Peak == snapshot->kind ? "peak" : "detailed" ));
      pp_snapshot_SXPt(fd, snapshot->alloc_sxpt, 0, depth_str,
                       depth_str_len, snapshot->heap_szB,
                       snapshot_total_szB);

      VG_(free)(depth_str);

   } else {
      FP("heap_tree=empty\n");
   }
}

static void write_snapshots_to_file(void)
{
   Int i, fd;
   SysRes sres;

   // Setup output filename.  Nb: it's important to do this now, ie. as late
   // as possible.  If we do it at start-up and the program forks and the
   // output file format string contains a %p (pid) specifier, both the
   // parent and child will incorrectly write to the same file;  this
   // happened in 3.3.0.
   Char* massif_out_file =
      VG_(expand_file_name)("--massif-out-file", clo_massif_out_file);

   sres = VG_(open)(massif_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                                     VKI_S_IRUSR|VKI_S_IWUSR);
   if (sr_isError(sres)) {
      // If the file can't be opened for whatever reason (conflict
      // between multiple cachegrinded processes?), give up now.
      VG_(umsg)("error: can't open output file '%s'\n", massif_out_file );
      VG_(umsg)("       ... so profiling results will be missing.\n");
      VG_(free)(massif_out_file);
      return;
   } else {
      fd = sr_Res(sres);
      VG_(free)(massif_out_file);
   }

   // Print massif-specific options that were used.
   // XXX: is it worth having a "desc:" line?  Could just call it "options:"
   // -- this file format isn't as generic as Cachegrind's, so the
   // implied genericity of "desc:" is bogus.
   FP("desc:");
   for (i = 0; i < VG_(sizeXA)(args_for_massif); i++) {
      Char* arg = *(Char**)VG_(indexXA)(args_for_massif, i);
      FP(" %s", arg);
   }
   if (0 == i) FP(" (none)");
   FP("\n");

   // Print "cmd:" line.
   FP("cmd: ");
   if (VG_(args_the_exename)) {
      FP("%s", VG_(args_the_exename));
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         HChar* arg = * (HChar**) VG_(indexXA)( VG_(args_for_client), i );
         if (arg)
            FP(" %s", arg);
      }
   } else {
      FP(" ???");
   }
   FP("\n");

   FP("time_unit: %s\n", TimeUnit_to_string(clo_time_unit));

   for (i = 0; i < next_snapshot_i; i++) {
      Snapshot* snapshot = & snapshots[i];
      pp_snapshot(fd, snapshot, i);     // Detailed snapshot!
   }
}


//------------------------------------------------------------//
//--- Finalisation                                         ---//
//------------------------------------------------------------//

static void ms_fini(Int exit_status)
{
   // Output.
   write_snapshots_to_file();

   // Stats
   tl_assert(n_xpts > 0);  // always have alloc_xpt
   STATS("heap allocs:           %u\n", n_heap_allocs);
   STATS("heap reallocs:         %u\n", n_heap_reallocs);
   STATS("heap frees:            %u\n", n_heap_frees);
   STATS("ignored heap allocs:   %u\n", n_ignored_heap_allocs);
   STATS("ignored heap frees:    %u\n", n_ignored_heap_frees);
   STATS("ignored heap reallocs: %u\n", n_ignored_heap_reallocs);
   STATS("stack allocs:          %u\n", n_stack_allocs);
   STATS("stack frees:           %u\n", n_stack_frees);
   STATS("XPts:                  %u\n", n_xpts);
   STATS("top-XPts:              %u (%d%%)\n",
      alloc_xpt->n_children,
      ( n_xpts ? alloc_xpt->n_children * 100 / n_xpts : 0));
   STATS("XPt init expansions:   %u\n", n_xpt_init_expansions);
   STATS("XPt later expansions:  %u\n", n_xpt_later_expansions);
   STATS("SXPt allocs:           %u\n", n_sxpt_allocs);
   STATS("SXPt frees:            %u\n", n_sxpt_frees);
   STATS("skipped snapshots:     %u\n", n_skipped_snapshots);
   STATS("real snapshots:        %u\n", n_real_snapshots);
   STATS("detailed snapshots:    %u\n", n_detailed_snapshots);
   STATS("peak snapshots:        %u\n", n_peak_snapshots);
   STATS("cullings:              %u\n", n_cullings);
   STATS("XCon redos:            %u\n", n_XCon_redos);
}


//------------------------------------------------------------//
//--- Initialisation                                       ---//
//------------------------------------------------------------//

static void ms_post_clo_init(void)
{
   Int i;
   Char* LD_PRELOAD_val;
   Char* s;
   Char* s2;

   // Check options.
   if (clo_pages_as_heap) {
      if (clo_stacks) {
         VG_(fmsg_bad_option)(
            "--pages-as-heap=yes together with --stacks=yes", "");
      }
   }
   if (!clo_heap) {
      clo_pages_as_heap = False;
   }

   // If --pages-as-heap=yes we don't want malloc replacement to occur.  So we
   // disable vgpreload_massif-$PLATFORM.so by removing it from LD_PRELOAD (or
   // platform-equivalent).  We replace it entirely with spaces because then
   // the linker doesn't complain (it does complain if we just change the name
   // to a bogus file).  This is a bit of a hack, but LD_PRELOAD is setup well
   // before tool initialisation, so this seems the best way to do it.
   if (clo_pages_as_heap) {
      clo_heap_admin = 0;     // No heap admin on pages.

      LD_PRELOAD_val = VG_(getenv)( (Char*)VG_(LD_PRELOAD_var_name) );
      tl_assert(LD_PRELOAD_val);

      // Make sure the vgpreload_core-$PLATFORM entry is there, for sanity.
      s2 = VG_(strstr)(LD_PRELOAD_val, "vgpreload_core");
      tl_assert(s2);

      // Now find the vgpreload_massif-$PLATFORM entry.
      s2 = VG_(strstr)(LD_PRELOAD_val, "vgpreload_massif");
      tl_assert(s2);

      // Blank out everything to the previous ':', which must be there because
      // of the preceding vgpreload_core-$PLATFORM entry.
      for (s = s2; *s != ':'; s--) {
         *s = ' ';
      }

      // Blank out everything to the end of the entry, which will be '\0' if
      // LD_PRELOAD was empty before Valgrind started, or ':' otherwise.
      for (s = s2; *s != ':' && *s != '\0'; s++) {
         *s = ' ';
      }
   }

   // Print alloc-fns and ignore-fns, if necessary.
   if (VG_(clo_verbosity) > 1) {
      VERB(1, "alloc-fns:\n");
      for (i = 0; i < VG_(sizeXA)(alloc_fns); i++) {
         Char** fn_ptr = VG_(indexXA)(alloc_fns, i);
         VERB(1, "  %s\n", *fn_ptr);
      }

      VERB(1, "ignore-fns:\n");
      if (0 == VG_(sizeXA)(ignore_fns)) {
         VERB(1, "  <empty>\n");
      }
      for (i = 0; i < VG_(sizeXA)(ignore_fns); i++) {
         Char** fn_ptr = VG_(indexXA)(ignore_fns, i);
         VERB(1, "  %d: %s\n", i, *fn_ptr);
      }
   }

   // Events to track.
   if (clo_stacks) {
      VG_(track_new_mem_stack)        ( new_mem_stack        );
      VG_(track_die_mem_stack)        ( die_mem_stack        );
      VG_(track_new_mem_stack_signal) ( new_mem_stack_signal );
      VG_(track_die_mem_stack_signal) ( die_mem_stack_signal );
   }

   if (clo_pages_as_heap) {
      VG_(track_new_mem_startup) ( ms_new_mem_startup );
      VG_(track_new_mem_brk)     ( ms_new_mem_brk     );
      VG_(track_new_mem_mmap)    ( ms_new_mem_mmap    );

      VG_(track_copy_mem_remap)  ( ms_copy_mem_remap  );

      VG_(track_die_mem_brk)     ( ms_die_mem_brk     );
      VG_(track_die_mem_munmap)  ( ms_die_mem_munmap  ); 
   }

   // Initialise snapshot array, and sanity-check it.
   snapshots = VG_(malloc)("ms.main.mpoci.1", 
                           sizeof(Snapshot) * clo_max_snapshots);
   // We don't want to do snapshot sanity checks here, because they're
   // currently uninitialised.
   for (i = 0; i < clo_max_snapshots; i++) {
      clear_snapshot( & snapshots[i], /*do_sanity_check*/False );
   }
   sanity_check_snapshots_array();
}

static void ms_pre_clo_init(void)
{
   VG_(details_name)            ("Massif");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a heap profiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2003-2010, and GNU GPL'd, by Nicholas Nethercote");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   // Basic functions.
   VG_(basic_tool_funcs)          (ms_post_clo_init,
                                   ms_instrument,
                                   ms_fini);

   // Needs.
   VG_(needs_libc_freeres)();
   VG_(needs_command_line_options)(ms_process_cmd_line_option,
                                   ms_print_usage,
                                   ms_print_debug_usage);
   VG_(needs_client_requests)     (ms_handle_client_request);
   VG_(needs_sanity_checks)       (ms_cheap_sanity_check,
                                   ms_expensive_sanity_check);
   VG_(needs_malloc_replacement)  (ms_malloc,
                                   ms___builtin_new,
                                   ms___builtin_vec_new,
                                   ms_memalign,
                                   ms_calloc,
                                   ms_free,
                                   ms___builtin_delete,
                                   ms___builtin_vec_delete,
                                   ms_realloc,
                                   ms_malloc_usable_size,
                                   0 );

   // HP_Chunks.
   malloc_list = VG_(HT_construct)( "Massif's malloc list" );

   // Dummy node at top of the context structure.
   alloc_xpt = new_XPt(/*ip*/0, /*parent*/NULL);

   // Initialise alloc_fns and ignore_fns.
   init_alloc_fns();
   init_ignore_fns();

   // Initialise args_for_massif.
   args_for_massif = VG_(newXA)(VG_(malloc), "ms.main.mprci.1", 
                                VG_(free), sizeof(HChar*));
}

VG_DETERMINE_INTERFACE_VERSION(ms_pre_clo_init)

//--------------------------------------------------------------------//
//--- end                                                          ---//
//--------------------------------------------------------------------//
