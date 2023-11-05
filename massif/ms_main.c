//--------------------------------------------------------------------//
//--- Massif: a heap profiling tool.                     ms_main.c ---//
//--------------------------------------------------------------------//

/*
   This file is part of Massif, a Valgrind tool for profiling memory
   usage of programs.

   Copyright (C) 2003-2017 Nicholas Nethercote
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

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
// - do snapshots on some specific client requests
//     - "show me the extra allocations since the last snapshot"
//     - "start/stop logging" (eg. quickly skip boring bits)
// - Add ability to draw multiple graphs, eg. heap-only, stack-only, total.
//   Give each graph a title.  (try to do it generically!)
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
#include "pub_tool_poolalloc.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_stacktrace.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"
#include "pub_tool_xtree.h"
#include "pub_tool_xtmemory.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_gdbserver.h"

#include "pub_tool_clreq.h"           // For {MALLOC,FREE}LIKE_BLOCK

//------------------------------------------------------------*/
//--- Overview of operation                                ---*/
//------------------------------------------------------------*/

// The size of the stacks and heap is tracked.  The heap is tracked in a lot
// of detail, enough to tell how many bytes each line of code is responsible
// for, more or less.  The main data structure is an xtree maintaining the
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
   if (UNLIKELY(VG_(clo_verbosity) > verb)) { \
      VG_(dmsg)("Massif: " format, ##args);   \
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

static UInt n_skipped_snapshots     = 0;
static UInt n_real_snapshots        = 0;
static UInt n_detailed_snapshots    = 0;
static UInt n_peak_snapshots        = 0;
static UInt n_cullings              = 0;

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

// alloc_fns is not used for detecting allocations
// it is used when checking for ignore functions in the callstack
// see filter_IPs
// allocation detection uses the usual coregrind reaplace malloc
// mechanism which calls ms_malloc etc. here and in the end
// everything goes through alloc_and_record_block
static XArray* alloc_fns;
static XArray* ignore_fns;

static void init_alloc_fns(void)
{
   // Create the list, and add the default elements.
   alloc_fns = VG_(newXA)(VG_(malloc), "ms.main.iaf.1",
                                       VG_(free), sizeof(HChar*));
   #define DO(x)  { const HChar* s = x; VG_(addToXA)(alloc_fns, &s); }

   // Ordered roughly according to (presumed) frequency.
   // Nb: The C++ "operator new*" ones are overloadable.  We include them
   // always anyway, because even if they're overloaded, it would be a
   // prodigiously stupid overloading that caused them to not allocate
   // memory.
   //
   // PJF: the above comment is a bit wide of the mark.
   // See https://en.cppreference.com/w/cpp/memory/new/operator_new
   // There are two "non-allocating placement allocation functions"
   //
   // Because of the above we can't use wildcards.
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
# if VG_WORDSIZE == 4
   DO("operator new(unsigned)"                              );
#else
   DO("operator new(unsigned long)"                         );
#endif
   DO("__builtin_vec_new"                                   );
# if VG_WORDSIZE == 4
   DO("operator new[](unsigned)"                            );
#else
   DO("operator new[](unsigned long)"                       );
#endif
   DO("calloc"                                              );
   DO("aligned_alloc"                                       );
   DO("realloc"                                             );
   DO("memalign"                                            );
   DO("posix_memalign"                                      );
   DO("valloc"                                              );
# if VG_WORDSIZE == 4
   DO("operator new(unsigned, std::nothrow_t const&)"       );
   DO("operator new[](unsigned, std::nothrow_t const&)"     );
   DO("operator new(unsigned, std::align_val_t)"            );
   DO("operator new[](unsigned, std::align_val_t)"          );
   DO("operator new(unsigned, std::align_val_t, std::nothrow_t const&)"   );
   DO("operator new[](unsigned, std::align_val_t, std::nothrow_t const&)" );
#else
   DO("operator new(unsigned long, std::nothrow_t const&)"  );
   DO("operator new[](unsigned long, std::nothrow_t const&)");
   DO("operator new(unsigned long, std::align_val_t)"       );
   DO("operator new[](unsigned long, std::align_val_t)"     );
   DO("operator new(unsigned long, std::align_val_t, std::nothrow_t const&)"   );
   DO("operator new[](unsigned long, std::align_val_t, std::nothrow_t const&)" );
#endif
#if defined(VGO_darwin)
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
                                        VG_(free), sizeof(HChar*));
}

//------------------------------------------------------------//
//--- Command line args                                    ---//
//------------------------------------------------------------//

#define MAX_DEPTH       200

typedef enum { TimeI, TimeMS, TimeB } TimeUnit;

static const HChar* TimeUnit_to_string(TimeUnit time_unit)
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
static const HChar* clo_massif_out_file = "massif.out.%p";

static XArray* args_for_massif;

static Bool ms_process_cmd_line_option(const HChar* arg)
{
   const HChar* tmp_str;

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
//--- XTrees                                               ---//
//------------------------------------------------------------//

// The details of the heap are represented by a single XTree.
// This XTree maintains the nr of allocated bytes for each
// stacktrace/execontext.
//
// The root of the Xtree will be output as a top node  'alloc functions',
//  which represents all allocation functions, eg:
// - malloc/calloc/realloc/memalign/new/new[];
// - user-specified allocation functions (using --alloc-fn);
// - custom allocation (MALLOCLIKE) points
static XTree* heap_xt;
/* heap_xt contains a SizeT: the nr of allocated bytes by this execontext. */
static void init_szB(void* value)
{
   *((SizeT*)value) = 0;
}
static void add_szB(void* to, const void* value)
{
   *((SizeT*)to) += *((const SizeT*)value);
}
static void sub_szB(void* from, const void* value)
{
   *((SizeT*)from) -= *((const SizeT*)value);
}
static ULong alloc_szB(const void* value)
{
   return (ULong)*((const SizeT*)value);
}


//------------------------------------------------------------//
//--- XTree Operations                                     ---//
//------------------------------------------------------------//

// This is the limit on the number of filtered alloc-fns that can be in a
// single stacktrace.
#define MAX_OVERESTIMATE   50
#define MAX_IPS            (MAX_DEPTH + MAX_OVERESTIMATE)

// filtering out uninteresting entries:
// alloc-fns and entries above alloc-fns, and entries below main-or-below-main.
//   Eg:       alloc-fn1 / alloc-fn2 / a / b / main / (below main) / c
//   becomes:  a / b / main
// Nb: it's possible to end up with an empty trace, eg. if 'main' is marked
// as an alloc-fn.  This is ok.
static
void filter_IPs (Addr* ips, Int n_ips,
                 UInt* top, UInt* n_ips_sel)
{
   Int i;
   Bool top_has_fnname = False;
   Bool is_alloc_fn = False;
   Bool is_inline_fn = False;
   const HChar *fnname;

   *top = 0;
   *n_ips_sel = n_ips;

   // Advance *top as long as we find alloc functions
   // PW Nov 2016 xtree work:
   //  old massif code was doing something really strange(?buggy):
   //  'sliding' a bunch of functions without names by removing an
   //  alloc function 'inside' a stacktrace e.g.
   //    0x1 0x2 0x3 alloc func1 main
   //  became   0x1 0x2 0x3 func1 main
   const DiEpoch ep = VG_(current_DiEpoch)();
   InlIPCursor *iipc = NULL;

   for (i = *top; i < n_ips; ++i) {
      iipc = VG_(new_IIPC)(ep, ips[i]);
      do {
         top_has_fnname = VG_(get_fnname_inl)(ep, ips[i], &fnname, iipc);
         is_alloc_fn = top_has_fnname && VG_(strIsMemberXA)(alloc_fns, fnname);
         is_inline_fn = VG_(next_IIPC)(iipc);
         if (is_alloc_fn && is_inline_fn) {
            VERB(4, "filtering inline alloc fn %s\n", fnname);
         }
      } while (is_alloc_fn && is_inline_fn);
      VG_(delete_IIPC)(iipc);

      if (is_alloc_fn) {
         VERB(4, "filtering alloc fn %s\n", fnname);
         (*top)++;
         (*n_ips_sel)--;
      } else {
         break;
      }
   }

   // filter the whole stacktrace if this allocation has to be ignored.
   if (*n_ips_sel > 0 && VG_(sizeXA)(ignore_fns) > 0) {
      if (!top_has_fnname) {
         // top has no fnname => search for the first entry that has a fnname
         for (i = *top; i < n_ips && !top_has_fnname; ++i) {
            iipc = VG_(new_IIPC)(ep, ips[i]);
            do {
               top_has_fnname = VG_(get_fnname_inl)(ep, ips[i], &fnname, iipc);
               if (top_has_fnname) {
                  break;
               }
            } while (VG_(next_IIPC)(iipc));
            VG_(delete_IIPC)(iipc);
         }
      }
      if (top_has_fnname && VG_(strIsMemberXA)(ignore_fns, fnname)) {
         VERB(4, "ignored allocation from fn %s\n", fnname);
         *top = n_ips;
         *n_ips_sel = 0;
      }
   }

   if (!VG_(clo_show_below_main) && *n_ips_sel > 0 ) {
      // Technically, it would be better to use the 'real' epoch that
      // was used to capture ips/n_ips. However, this searches
      // for a main or below_main function. It is technically possible
      // but unlikely that main or below main fn is in a dlclose-d library,
      // so current epoch is reasonable enough, even if not perfect.
      // FIXME PW EPOCH: would be better to also use the real ips epoch here,
      // once m_xtree.c massif output format properly supports epoch.
      const DiEpoch cur_ep = VG_(current_DiEpoch)();
      Int mbm = VG_(XT_offset_main_or_below_main)(cur_ep, ips, n_ips);

      if (mbm < *top) {
         // Special case: the first main (or below main) function is an
         // alloc function.
         *n_ips_sel = 1;
         VERB(4, "main/below main: keeping 1 fn\n");
      } else {
         *n_ips_sel -= n_ips - mbm - 1;
         VERB(4, "main/below main: filtering %d\n", n_ips - mbm - 1);
      }
   }

   // filter the frames if we have more than clo_depth
   if (*n_ips_sel > clo_depth) {
      VERB(4, "filtering IPs above clo_depth\n");
      *n_ips_sel = clo_depth;
   }
}

// Capture a stacktrace, and make an ec of it, without the first entry
// if exclude_first_entry is True.
static ExeContext* make_ec(ThreadId tid, Bool exclude_first_entry)
{
   static Addr ips[MAX_IPS];

   // After this call, the IPs we want are in ips[0]..ips[n_ips-1].
   Int n_ips = VG_(get_StackTrace)( tid, ips, clo_depth +  MAX_OVERESTIMATE,
                                    NULL/*array to dump SP values in*/,
                                    NULL/*array to dump FP values in*/,
                                    0/*first_ip_delta*/ );
   if (exclude_first_entry) {
      if (n_ips > 1) {
         const HChar *fnname;
         VERB(4, "removing top fn %s from stacktrace\n",
              VG_(get_fnname)(VG_(current_DiEpoch)(), ips[0], &fnname)
              ? fnname : "???");
         return VG_(make_ExeContext_from_StackTrace)(ips+1, n_ips-1);
      } else {
         VERB(4, "null execontext as removing top fn with n_ips %d\n", n_ips);
         return VG_(null_ExeContext) ();
      }
   } else
      return VG_(make_ExeContext_from_StackTrace)(ips, n_ips);
}

// Create (or update) in heap_xt an xec corresponding to the stacktrace of tid.
// req_szB is added to the xec (unless ec is fully filtered).
// Returns the correspding XTree xec.
// exclude_first_entry is an optimisation: if True, automatically removes
// the top level IP from the stacktrace. Should be set to True if it is known
// that this is an alloc fn. The top function presumably will be something like
// malloc or __builtin_new that we're sure to filter out).
static Xecu add_heap_xt( ThreadId tid, SizeT req_szB, Bool exclude_first_entry)
{
   ExeContext *ec = make_ec(tid, exclude_first_entry);

   if (UNLIKELY(VG_(clo_xtree_memory) == Vg_XTMemory_Full))
      VG_(XTMemory_Full_alloc)(req_szB, ec);
   return VG_(XT_add_to_ec) (heap_xt, ec, &req_szB);
}

// Substract req_szB from the heap_xt where.
static void sub_heap_xt(Xecu where, SizeT req_szB, Bool exclude_first_entry)
{
   tl_assert(clo_heap);

   if (0 == req_szB)
      return;

   VG_(XT_sub_from_xecu) (heap_xt, where, &req_szB);
   if (UNLIKELY(VG_(clo_xtree_memory) == Vg_XTMemory_Full)) {
      ExeContext *ec_free = make_ec(VG_(get_running_tid)(),
                                    exclude_first_entry);
      VG_(XTMemory_Full_free)(req_szB,
                              VG_(XT_get_ec_from_xecu)(heap_xt, where),
                              ec_free);
   }
}


//------------------------------------------------------------//
//--- Snapshots                                            ---//
//------------------------------------------------------------//

// Snapshots are done in a way so that we always have a reasonable number of
// them.  We start by taking them quickly.  Once we hit our limit, we cull
// some (eg. half), and start taking them more slowly.  Once we hit the
// limit again, we again cull and then take them even more slowly, and so
// on.

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
      XTree* xt;    // Snapshot of heap_xt, if a detailed snapshot,
   }                // otherwise NULL.
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
      tl_assert(snapshot->xt             == NULL);
      return False;
   } else {
      tl_assert(snapshot->time           != UNUSED_SNAPSHOT_TIME);
      return True;
   }
}

static Bool is_detailed_snapshot(Snapshot* snapshot)
{
   return (snapshot->xt ? True : False);
}

static Bool is_uncullable_snapshot(Snapshot* snapshot)
{
   return &snapshots[0] == snapshot                   // First snapshot
       || &snapshots[next_snapshot_i-1] == snapshot   // Last snapshot
       || snapshot->kind == Peak;                     // Peak snapshot
}

static void sanity_check_snapshot(Snapshot* snapshot)
{
   // Not much we can sanity check.
   tl_assert(snapshot->xt == NULL || snapshot->kind != Unused);
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

// This zeroes all the fields in the snapshot, but does not free the xt
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
   snapshot->xt             = NULL;
}

// This zeroes all the fields in the snapshot, and frees the heap XTree xt if
// present.
static void delete_snapshot(Snapshot* snapshot)
{
   // Nb: if there's an XTree, we free it after calling clear_snapshot,
   // because clear_snapshot does a sanity check which includes checking the
   // XTree.
   XTree* tmp_xt = snapshot->xt;
   clear_snapshot(snapshot, /*do_sanity_check*/True);
   if (tmp_xt) {
       VG_(XT_delete)(tmp_xt);
   }
}

static void VERB_snapshot(Int verbosity, const HChar* prefix, Int i)
{
   Snapshot* snapshot = &snapshots[i];
   const HChar* suffix;
   switch (snapshot->kind) {
   case Peak:   suffix = "p";                                            break;
   case Normal: suffix = ( is_detailed_snapshot(snapshot) ? "d" : "." ); break;
   case Unused: suffix = "u";                                            break;
   default:
      tl_assert2(0, "VERB_snapshot: unknown snapshot kind: %d", snapshot->kind);
   }
   VERB(verbosity, "%s S%s%3d (t:%lld, hp:%lu, ex:%lu, st:%lu)\n",
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
         HChar buf[64];   // large enough
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
         snapshot->xt = VG_(XT_snapshot)(heap_xt);
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
maybe_take_snapshot(SnapshotKind kind, const HChar* what)
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
   tl_assert(heap_xt);
   sanity_check_snapshots_array();
   return True;
}


//------------------------------------------------------------//
//--- Heap management                                      ---//
//------------------------------------------------------------//

// Metadata for heap blocks.  Each one contains an Xecu,
// which identifies the XTree ec at which it was allocated.  From
// HP_Chunks, XTree ec 'space' field is incremented (at allocation) and
// decremented (at deallocation).
//
// Nb: first two fields must match core's VgHashNode.
typedef
   struct _HP_Chunk {
      struct _HP_Chunk* next;
      Addr              data;       // Ptr to actual block
      SizeT             req_szB;    // Size requested
      SizeT             slop_szB;   // Extra bytes given above those requested
      Xecu              where;      // Where allocated; XTree xecu from heap_xt
   }
   HP_Chunk;

/* Pool allocator for HP_Chunk. */   
static PoolAlloc *HP_chunk_poolalloc = NULL;

static VgHashTable *malloc_list  = NULL;   // HP_Chunks

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
   HP_Chunk* hc = VG_(allocEltPA)(HP_chunk_poolalloc);
   hc->req_szB  = req_szB;
   hc->slop_szB = slop_szB;
   hc->data     = (Addr)p;
   hc->where    = 0;
   VG_(HT_add_node)(malloc_list, hc);

   if (clo_heap) {
      VERB(3, "<<< record_block (%lu, %lu)\n", req_szB, slop_szB);

      hc->where = add_heap_xt( tid, req_szB, exclude_first_entry);

      if (VG_(XT_n_ips_sel)(heap_xt, hc->where) > 0) {
         // Update statistics.
         n_heap_allocs++;

         // Update heap stats.
         update_heap_stats(req_szB, clo_heap_admin + slop_szB);

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
   actual_szB = VG_(cli_malloc_usable_size)(p);
   tl_assert(actual_szB >= req_szB);
   slop_szB = actual_szB - req_szB;

   // Record block.
   record_block(tid, p, req_szB, slop_szB, /*exclude_first_entry*/True,
                /*maybe_snapshot*/True);

   return p;
}

static __inline__
void unrecord_block ( void* p, Bool maybe_snapshot, Bool exclude_first_entry )
{
   // Remove HP_Chunk from malloc_list
   HP_Chunk* hc = VG_(HT_remove)(malloc_list, (UWord)p);
   if (NULL == hc) {
      return;   // must have been a bogus free()
   }

   if (clo_heap) {
      VERB(3, "<<< unrecord_block\n");

      if (VG_(XT_n_ips_sel)(heap_xt, hc->where) > 0) {
         // Update statistics.
         n_heap_frees++;

         // Maybe take a peak snapshot, since it's a deallocation.
         if (maybe_snapshot) {
            maybe_take_snapshot(Peak, "de-PEAK");
         }

         // Update heap stats.
         update_heap_stats(-hc->req_szB, -clo_heap_admin - hc->slop_szB);

         // Update XTree.
         sub_heap_xt(hc->where, hc->req_szB, exclude_first_entry);

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
   VG_(freeEltPA) (HP_chunk_poolalloc, hc);  hc = NULL;
}

// Nb: --ignore-fn is tricky for realloc.  If the block's original alloc was
// ignored, but the realloc is not requested to be ignored, and we are
// shrinking the block, then we have to ignore the realloc -- otherwise we
// could end up with negative heap sizes.  This isn't a danger if we are
// growing such a block, but for consistency (it also simplifies things) we
// ignore such reallocs as well.
// PW Nov 2016 xtree work: why can't we just consider that a realloc of an
// ignored  alloc is just a new alloc (i.e. do not remove the old sz from the
// stats). Then everything would be fine, and a non ignored realloc would be
// counted properly.
static __inline__
void* realloc_block ( ThreadId tid, void* p_old, SizeT new_req_szB )
{
   HP_Chunk* hc;
   void*     p_new;
   SizeT     old_req_szB, old_slop_szB, new_slop_szB, new_actual_szB;
   Xecu      old_where;
   Bool      is_ignored = False;

   if (p_old == NULL) {
      return alloc_and_record_block( tid, new_req_szB, VG_(clo_alignment), /*is_zeroed*/False );
   }

   if (new_req_szB == 0U) {
      if (VG_(clo_realloc_zero_bytes_frees) == True) {
         /* like ms_free */
         unrecord_block(p_old, /*maybe_snapshot*/True, /*exclude_first_entry*/True);
         VG_(cli_free)(p_old);
         return NULL;
      }
      new_req_szB = 1U;
   }

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

      if (VG_(XT_n_ips_sel)(heap_xt, hc->where) > 0) {
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
      VG_(memcpy)(p_new, p_old, old_req_szB + old_slop_szB);
      VG_(cli_free)(p_old);
      new_actual_szB = VG_(cli_malloc_usable_size)(p_new);
      tl_assert(new_actual_szB >= new_req_szB);
      new_slop_szB = new_actual_szB - new_req_szB;
   }

   if (p_new) {
      // Update HP_Chunk.
      hc->data     = (Addr)p_new;
      hc->req_szB  = new_req_szB;
      hc->slop_szB = new_slop_szB;
      old_where    = hc->where;
      hc->where    = 0;

      // Update XTree.
      if (clo_heap) {
         hc->where = add_heap_xt( tid, new_req_szB,
                                  /*exclude_first_entry*/True);
         if (!is_ignored && VG_(XT_n_ips_sel)(heap_xt, hc->where) > 0) {
            sub_heap_xt(old_where, old_req_szB, /*exclude_first_entry*/True);
         } else {
            // The realloc itself is ignored.
            is_ignored = True;

            /* XTREE??? hack to have something compatible with pre
               m_xtree massif: if the previous alloc/realloc was
               ignored, and this one is not ignored, then keep the
               previous where, to continue marking this memory as
               ignored. */
            if (VG_(XT_n_ips_sel)(heap_xt, hc->where) > 0
                && VG_(XT_n_ips_sel)(heap_xt, old_where) == 0)
               hc->where = old_where;

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
           (SSizeT)(new_req_szB - old_req_szB),
           (SSizeT)(new_slop_szB - old_slop_szB));
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

static void* ms___builtin_new_aligned ( ThreadId tid, SizeT szB, SizeT alignB , SizeT orig_alignB )
{
   return alloc_and_record_block( tid, szB, alignB, /*is_zeroed*/False );
}

static void* ms___builtin_vec_new ( ThreadId tid, SizeT szB )
{
   return alloc_and_record_block( tid, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* ms___builtin_vec_new_aligned ( ThreadId tid, SizeT szB, SizeT alignB, SizeT orig_alignB )
{
   return alloc_and_record_block( tid, szB, alignB, /*is_zeroed*/False );
}

static void* ms_calloc ( ThreadId tid, SizeT m, SizeT szB )
{
   return alloc_and_record_block( tid, m*szB, VG_(clo_alignment), /*is_zeroed*/True );
}

static void *ms_memalign ( ThreadId tid, SizeT alignB, SizeT orig_alignB, SizeT szB)
{
   return alloc_and_record_block( tid, szB, alignB, False );
}

static void ms_free ( ThreadId tid __attribute__((unused)), void* p )
{
   unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/True);
   VG_(cli_free)(p);
}

static void ms___builtin_delete ( ThreadId tid, void* p )
{
   unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/True);
   VG_(cli_free)(p);
}

static void ms___builtin_delete_aligned ( ThreadId tid, void* p, SizeT align )
{
   unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/True);
   VG_(cli_free)(p);
}

static void ms___builtin_vec_delete ( ThreadId tid, void* p )
{
   unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/True);
   VG_(cli_free)(p);
}

static void ms___builtin_vec_delete_aligned ( ThreadId tid, void* p, SizeT align )
{
   unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/True);
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
   // Unrecord the first page. This might be the peak, so do a snapshot.
   unrecord_block((void*)a, /*maybe_snapshot*/True,
                  /*exclude_first_entry*/False);
   a += VKI_PAGE_SIZE;
   // Then unrecord the remaining pages, but without snapshots.
   for (end = a + len - VKI_PAGE_SIZE; a < end; a += VKI_PAGE_SIZE) {
      unrecord_block((void*)a, /*maybe_snapshot*/False,
                     /*exclude_first_entry*/False);
   }
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
   // brk limit is not necessarily aligned on a page boundary.
   // If new memory being brk-ed implies to allocate a new page,
   // then call ms_record_page_mem with page aligned parameters
   // otherwise just ignore.
   Addr old_bottom_page = VG_PGROUNDDN(a - 1);
   Addr new_top_page = VG_PGROUNDDN(a + len - 1);
   if (old_bottom_page != new_top_page)
      ms_record_page_mem(VG_PGROUNDDN(a),
                         (new_top_page - old_bottom_page));
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
   // Call ms_unrecord_page_mem only if one or more pages are de-allocated.
   // See ms_new_mem_brk for more details.
   Addr new_bottom_page = VG_PGROUNDDN(a - 1);
   Addr old_top_page = VG_PGROUNDDN(a + len - 1);
   if (old_top_page != new_bottom_page)
      ms_unrecord_page_mem(VG_PGROUNDDN(a),
                           (old_top_page - new_bottom_page));

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

static INLINE void new_mem_stack_2(SizeT len, const HChar* what)
{
   if (have_started_executing_code) {
      VERB(3, "<<< new_mem_stack (%lu)\n", len);
      n_stack_allocs++;
      update_stack_stats(len);
      maybe_take_snapshot(Normal, what);
      VERB(3, ">>>\n");
   }
}

static INLINE void die_mem_stack_2(SizeT len, const HChar* what)
{
   if (have_started_executing_code) {
      VERB(3, "<<< die_mem_stack (-%lu)\n", len);
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

static void print_monitor_help ( void )
{
   VG_(gdb_printf) (
"\n"
"massif monitor commands:\n"
"  snapshot [<filename>]\n"
"  detailed_snapshot [<filename>]\n"
"      takes a snapshot (or a detailed snapshot)\n"
"      and saves it in <filename>\n"
"             default <filename> is massif.vgdb.out\n"
"  all_snapshots [<filename>]\n"
"      saves all snapshot(s) taken so far in <filename>\n"
"             default <filename> is massif.vgdb.out\n"
"  xtmemory [<filename>]\n"
"        dump xtree memory profile in <filename> (default xtmemory.kcg.%%p.%%n)\n"
"\n");
}


/* Forward declaration.
   return True if request recognised, False otherwise */
static Bool handle_gdb_monitor_command (ThreadId tid, HChar *req);
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
   case VG_USERREQ__RESIZEINPLACE_BLOCK: {
      void* p        = (void*)argv[1];
      SizeT newSizeB =       argv[3];

      unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/False);
      record_block(tid, p, newSizeB, /*slop_szB*/0,
                   /*exclude_first_entry*/False, /*maybe_snapshot*/True);
      return True;
   }
   case VG_USERREQ__FREELIKE_BLOCK: {
      void* p = (void*)argv[1];
      unrecord_block(p, /*maybe_snapshot*/True, /*exclude_first_entry*/False);
      *ret = 0;
      return True;
   }
   case VG_USERREQ__GDB_MONITOR_COMMAND: {
     Bool handled = handle_gdb_monitor_command (tid, (HChar*)argv[1]);
     if (handled)
       *ret = 1;
     else
       *ret = 0;
     return handled;
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
                      const VexGuestLayout* layout,
                      const VexGuestExtents* vge,
                      const VexArchInfo* archinfo_host,
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

static void pp_snapshot(MsFile *fp, Snapshot* snapshot, Int snapshot_n)
{
   const Massif_Header header = (Massif_Header) {
      .snapshot_n    = snapshot_n,
      .time          = snapshot->time,
      .sz_B          = snapshot->heap_szB,
      .extra_B       = snapshot->heap_extra_szB,
      .stacks_B      = snapshot->stacks_szB,
      .detailed      = is_detailed_snapshot(snapshot),
      .peak          = Peak == snapshot->kind,
      .top_node_desc = clo_pages_as_heap ?
        "(page allocation syscalls) mmap/mremap/brk, --alloc-fns, etc."
        : "(heap allocation functions) malloc/new/new[], --alloc-fns, etc.",
      .sig_threshold = clo_threshold
   };

   sanity_check_snapshot(snapshot);

   VG_(XT_massif_print)(fp, snapshot->xt, &header, alloc_szB);
}

static void write_snapshots_to_file(const HChar* massif_out_file, 
                                    Snapshot snapshots_array[], 
                                    Int nr_elements)
{
   Int i;
   MsFile *fp;

   fp = VG_(XT_massif_open)(massif_out_file,
                            NULL,
                            args_for_massif,
                            TimeUnit_to_string(clo_time_unit));
   if (fp == NULL)
      return; // Error reported by VG_(XT_massif_open)

   for (i = 0; i < nr_elements; i++) {
      Snapshot* snapshot = & snapshots_array[i];
      pp_snapshot(fp, snapshot, i);     // Detailed snapshot!
   }
   VG_(XT_massif_close) (fp);
}

static void write_snapshots_array_to_file(void)
{
   // Setup output filename.  Nb: it's important to do this now, ie. as late
   // as possible.  If we do it at start-up and the program forks and the
   // output file format string contains a %p (pid) specifier, both the
   // parent and child will incorrectly write to the same file;  this
   // happened in 3.3.0.
   HChar* massif_out_file =
      VG_(expand_file_name)("--massif-out-file", clo_massif_out_file);
   write_snapshots_to_file (massif_out_file, snapshots, next_snapshot_i);
   VG_(free)(massif_out_file);
}

static void handle_snapshot_monitor_command (const HChar *filename,
                                             Bool detailed)
{
   Snapshot snapshot;

   if (!clo_pages_as_heap && !have_started_executing_code) {
      // See comments of variable have_started_executing_code.
      VG_(gdb_printf) 
         ("error: cannot take snapshot before execution has started\n");
      return;
   }

   clear_snapshot(&snapshot, /* do_sanity_check */ False);
   take_snapshot(&snapshot, Normal, get_time(), detailed);
   write_snapshots_to_file ((filename == NULL) ? 
                            "massif.vgdb.out" : filename,
                            &snapshot,
                            1);
   delete_snapshot(&snapshot);
}

static void handle_all_snapshots_monitor_command (const HChar *filename)
{
   if (!clo_pages_as_heap && !have_started_executing_code) {
      // See comments of variable have_started_executing_code.
      VG_(gdb_printf) 
         ("error: cannot take snapshot before execution has started\n");
      return;
   }

   write_snapshots_to_file ((filename == NULL) ? 
                            "massif.vgdb.out" : filename,
                            snapshots, next_snapshot_i);
}

static void xtmemory_report_next_block(XT_Allocs* xta, ExeContext** ec_alloc)
{
   const HP_Chunk* hc = VG_(HT_Next)(malloc_list);
   if (hc) {
      xta->nbytes = hc->req_szB;
      xta->nblocks = 1;
      *ec_alloc = VG_(XT_get_ec_from_xecu)(heap_xt, hc->where);
   } else
      xta->nblocks = 0;
}
static void ms_xtmemory_report ( const HChar* filename, Bool fini )
{ 
   // Make xtmemory_report_next_block ready to be called.
   VG_(HT_ResetIter)(malloc_list);
   VG_(XTMemory_report)(filename, fini, xtmemory_report_next_block,
                        VG_(XT_filter_maybe_below_main));
   /* As massif already filters one top function, use as filter
      VG_(XT_filter_maybe_below_main). */
}

static Bool handle_gdb_monitor_command (ThreadId tid, HChar *req)
{
   HChar* wcmd;
   HChar s[VG_(strlen)(req) + 1]; /* copy for strtok_r */
   HChar *ssaveptr;

   VG_(strcpy) (s, req);

   wcmd = VG_(strtok_r) (s, " ", &ssaveptr);
   switch (VG_(keyword_id) ("help snapshot detailed_snapshot all_snapshots"
                            " xtmemory", 
                            wcmd, kwd_report_duplicated_matches)) {
   case -2: /* multiple matches */
      return True;
   case -1: /* not found */
      return False;
   case  0: /* help */
      print_monitor_help();
      return True;
   case  1: { /* snapshot */
      HChar* filename;
      filename = VG_(strtok_r) (NULL, " ", &ssaveptr);
      handle_snapshot_monitor_command (filename, False /* detailed */);
      return True;
   }
   case  2: { /* detailed_snapshot */
      HChar* filename;
      filename = VG_(strtok_r) (NULL, " ", &ssaveptr);
      handle_snapshot_monitor_command (filename, True /* detailed */);
      return True;
   }
   case  3: { /* all_snapshots */
      HChar* filename;
      filename = VG_(strtok_r) (NULL, " ", &ssaveptr);
      handle_all_snapshots_monitor_command (filename);
      return True;
   }
   case  4: { /* xtmemory */
      HChar* filename;
      filename = VG_(strtok_r) (NULL, " ", &ssaveptr);
      ms_xtmemory_report (filename, False);
      return True;
   }
   default: 
      tl_assert(0);
      return False;
   }
}

static void ms_print_stats (void)
{
#define STATS(format, args...) \
      VG_(dmsg)("Massif: " format, ##args)

   STATS("heap allocs:           %u\n", n_heap_allocs);
   STATS("heap reallocs:         %u\n", n_heap_reallocs);
   STATS("heap frees:            %u\n", n_heap_frees);
   STATS("ignored heap allocs:   %u\n", n_ignored_heap_allocs);
   STATS("ignored heap frees:    %u\n", n_ignored_heap_frees);
   STATS("ignored heap reallocs: %u\n", n_ignored_heap_reallocs);
   STATS("stack allocs:          %u\n", n_stack_allocs);
   STATS("skipped snapshots:     %u\n", n_skipped_snapshots);
   STATS("real snapshots:        %u\n", n_real_snapshots);
   STATS("detailed snapshots:    %u\n", n_detailed_snapshots);
   STATS("peak snapshots:        %u\n", n_peak_snapshots);
   STATS("cullings:              %u\n", n_cullings);
#undef STATS
}


//------------------------------------------------------------//
//--- Finalisation                                         ---//
//------------------------------------------------------------//

static void ms_fini(Int exit_status)
{
   ms_xtmemory_report(VG_(clo_xtree_memory_file), True);

   // Output.
   write_snapshots_array_to_file();

   if (VG_(clo_stats))
      ms_print_stats();
}


//------------------------------------------------------------//
//--- Initialisation                                       ---//
//------------------------------------------------------------//

static void ms_post_clo_init(void)
{
   Int i;
   HChar* LD_PRELOAD_val;

   /* We will record execontext up to clo_depth + overestimate and
      we will store this as ec => we need to increase the backtrace size
      if smaller than what we will store. */
   if (VG_(clo_backtrace_size) < clo_depth + MAX_OVERESTIMATE)
      VG_(clo_backtrace_size) = clo_depth + MAX_OVERESTIMATE;

   // Check options.
   if (clo_pages_as_heap) {
      if (clo_stacks) {
         VG_(fmsg_bad_option)("--pages-as-heap=yes",
            "Cannot be used together with --stacks=yes");
      }
   }
   if (!clo_heap) {
      clo_pages_as_heap = False;
   }

   // If --pages-as-heap=yes we don't want malloc replacement to occur.  So we
   // disable vgpreload_massif-$PLATFORM.so by removing it from LD_PRELOAD (or
   // platform-equivalent). This is a bit of a hack, but LD_PRELOAD is setup
   // well before tool initialisation, so this seems the best way to do it.
   if (clo_pages_as_heap) {
      HChar* s1;
      HChar* s2;

      clo_heap_admin = 0;     // No heap admin on pages.

      LD_PRELOAD_val = VG_(getenv)( VG_(LD_PRELOAD_var_name) );
      tl_assert(LD_PRELOAD_val);

      VERB(2, "clo_pages_as_heap orig LD_PRELOAD '%s'\n", LD_PRELOAD_val);

      // Make sure the vgpreload_core-$PLATFORM entry is there, for sanity.
      s1 = VG_(strstr)(LD_PRELOAD_val, "vgpreload_core");
      tl_assert(s1);

      // Now find the vgpreload_massif-$PLATFORM entry.
      s1 = VG_(strstr)(LD_PRELOAD_val, "vgpreload_massif");
      tl_assert(s1);
      s2 = s1;

      // Position s1 on the previous ':', which must be there because
      // of the preceding vgpreload_core-$PLATFORM entry.
      for (; *s1 != ':'; s1--)
         ;

      // Position s2 on the next ':' or \0
      for (; *s2 != ':' && *s2 != '\0'; s2++)
         ;

      // Move all characters from s2 to s1
      while ((*s1++ = *s2++))
         ;

      VERB(2, "clo_pages_as_heap cleaned LD_PRELOAD '%s'\n", LD_PRELOAD_val);
   }

   // Print alloc-fns and ignore-fns, if necessary.
   if (VG_(clo_verbosity) > 1) {
      VERB(1, "alloc-fns:\n");
      for (i = 0; i < VG_(sizeXA)(alloc_fns); i++) {
         HChar** fn_ptr = VG_(indexXA)(alloc_fns, i);
         VERB(1, "  %s\n", *fn_ptr);
      }

      VERB(1, "ignore-fns:\n");
      if (0 == VG_(sizeXA)(ignore_fns)) {
         VERB(1, "  <empty>\n");
      }
      for (i = 0; i < VG_(sizeXA)(ignore_fns); i++) {
         HChar** fn_ptr = VG_(indexXA)(ignore_fns, i);
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

   if (VG_(clo_xtree_memory) == Vg_XTMemory_Full)
      // Activate full xtree memory profiling.
      // As massif already filters one top function, use as filter
      // VG_(XT_filter_maybe_below_main).
      VG_(XTMemory_Full_init)(VG_(XT_filter_maybe_below_main));

}

static void ms_pre_clo_init(void)
{
   VG_(details_name)            ("Massif");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a heap profiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2003-2017, and GNU GPL'd, by Nicholas Nethercote");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   VG_(details_avg_translation_sizeB) ( 330 );

   VG_(clo_vex_control).iropt_register_updates_default
      = VG_(clo_px_file_backed)
      = VexRegUpdSpAtMemAccess; // overridable by the user.

   // Basic functions.
   VG_(basic_tool_funcs)          (ms_post_clo_init,
                                   ms_instrument,
                                   ms_fini);

   // Needs.
   VG_(needs_libc_freeres)();
   VG_(needs_cxx_freeres)();
   VG_(needs_command_line_options)(ms_process_cmd_line_option,
                                   ms_print_usage,
                                   ms_print_debug_usage);
   VG_(needs_client_requests)     (ms_handle_client_request);
   VG_(needs_sanity_checks)       (ms_cheap_sanity_check,
                                   ms_expensive_sanity_check);
   VG_(needs_print_stats)         (ms_print_stats);
   VG_(needs_malloc_replacement)  (ms_malloc,
                                   ms___builtin_new,
                                   ms___builtin_new_aligned,
                                   ms___builtin_vec_new,
                                   ms___builtin_vec_new_aligned,
                                   ms_memalign,
                                   ms_calloc,
                                   ms_free,
                                   ms___builtin_delete,
                                   ms___builtin_delete_aligned,
                                   ms___builtin_vec_delete,
                                   ms___builtin_vec_delete_aligned,
                                   ms_realloc,
                                   ms_malloc_usable_size,
                                   0 );

   // HP_Chunks.
   HP_chunk_poolalloc = VG_(newPA)
      (sizeof(HP_Chunk),
       1000,
       VG_(malloc),
       "massif MC_Chunk pool",
       VG_(free));
   malloc_list = VG_(HT_construct)( "Massif's malloc list" );

   // Heap XTree
   heap_xt = VG_(XT_create)(VG_(malloc),
                            "ms.xtrees",
                            VG_(free),
                            sizeof(SizeT),
                            init_szB, add_szB, sub_szB,
                            filter_IPs);

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
