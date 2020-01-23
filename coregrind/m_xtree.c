
/*--------------------------------------------------------------------*/
/*--- An xtree, tree of stacktraces with data            m_xtree.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2016-2017 Philippe Waroquiers

   This code generalises the XTree idea that was implemented in
   the massif tool in Valgrind versions <= 3.12, which is
      Copyright (C) 2005-2017 Nicholas Nethercote
       njn@valgrind.org

   The XTree implementation in this file is however implemented completely
   differently. Some code has been re-used for the production of the
   massif file header (e.g. FP_cmd function).

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

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_clientstate.h"
#include "pub_core_stacktrace.h"
#include "pub_core_execontext.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_hashtable.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_debuginfo.h"
#include "pub_core_deduppoolalloc.h"
#include "pub_core_xtree.h"    /* self */

#define DMSG(level, ...) (level <= VG_(debugLog_getLevel)() ?         \
                          VG_(dmsg)(__VA_ARGS__)                      \
                          : 0)

/* Defines the relevant part of an ec. This is shared between an xt
   and its snapshots (see XT_shared XArray of xec). */
typedef
   struct _xec {
     ExeContext* ec;
     UShort top;        // The first ips of ec to take into account.
     UShort n_ips_sel;  // The nr of ips from top to take into account.
   }
   xec;

/* XT_shared maintains the information shared between an XT and all
   its snapshots. */
typedef
   struct _XT_shared {
      UWord nrRef; /* nr of XTrees referencing this shared memory. */

      Alloc_Fn_t alloc_fn;                /* alloc fn (nofail) */
      const HChar* cc;                    /* cost centre for alloc */
      Free_Fn_t free_fn;                  /* free fn */

      /* The data associated to each ec is stored in 2 arrays:
           an xec array, shared between an xt and all its snapshots.
           a  data array, private to each XTree.
         For an ec with an ECU ecu, d4ecu2xecu[ecu/4] gives the offset in
         xec and data arrays where the ec information is located (this
         indirection is used to avoid huge xec and data arrays, in
         case an XTree contains data only for a small number of ec.
         The offset in the xec and data array is used as xtree ec unique
         id i.e. an xecu. */

      UInt  d4ecu2xecu_sz; /* size of d4ecu2xecu (in nr of elements). */
      UInt* d4ecu2xecu;

      /* ec information common to an xt and its snapshots. */
      XArray* xec; /* XArray of xec, indexed by xecu (== d4ecu2xecu[ecu/4]). */
   
      /* XArray of xecu, sorted by StackTrace ips[top..top+n_ips_sel-1].
         See ips_order_cmp. */
      XArray* ips_order_xecu;
   } XT_shared;

/* NO_OFFSET indicates in d4ecu2xecu  there is no data (yet) for this ec
   (with the index ecu/4). */
#define NO_OFFSET 0xffffffff

static XT_shared* new_XT_shared (Alloc_Fn_t alloc_fn,
                                 const  HChar* cc,
                                 void   (*free_fn)(void*))
{
   XT_shared* shared;

   vg_assert(alloc_fn);
   vg_assert(cc);
   vg_assert(free_fn);
   shared = alloc_fn(cc, sizeof(*shared));
   shared->nrRef = 0;
   shared->alloc_fn = alloc_fn;
   shared->cc = cc;
   shared->free_fn = free_fn;

   shared->d4ecu2xecu_sz = 0;
   shared->d4ecu2xecu = NULL;
   shared->xec = VG_(newXA)(alloc_fn, cc, free_fn, sizeof(xec));
   shared->ips_order_xecu = NULL; // Allocated when needed.

   return shared;
}

static void delete_XT_shared (XT_shared* shared)
{
   vg_assert(shared->nrRef == 0);
   shared->free_fn(shared->d4ecu2xecu);
   VG_(deleteXA)(shared->xec);
   if (shared->ips_order_xecu != NULL)
      VG_(deleteXA)(shared->ips_order_xecu);
   shared->free_fn(shared);
}

/* Compare 2 entries in ips_order_xecu by StackTrace elements.
   In case stack traces are of different length, an 'absent' ips is
   considered smaller than any other address. */
static XArray* xec_data_for_sort; // Needed to translate an xecu into an xec
static Int ips_order_cmp(const void* vleft, const void* vright)
{
   const Xecu left_xecu  = *(const Xecu*)vleft;
   const Xecu right_xecu = *(const Xecu*)vright;
   const xec* left  = VG_(indexXA)(xec_data_for_sort, left_xecu);
   const xec* right = VG_(indexXA)(xec_data_for_sort, right_xecu);
   const StackTrace left_ips  = VG_(get_ExeContext_StackTrace)(left->ec)
      + left->top;
   const StackTrace right_ips = VG_(get_ExeContext_StackTrace)(right->ec)
      + right->top;
   UInt i;

   const UInt c_n_ips_sel = left->n_ips_sel < right->n_ips_sel 
      ? left->n_ips_sel : right->n_ips_sel;

   // First see if we have a difference on the common nr of ips selected
   for (i = 0; i < c_n_ips_sel; i++) {
      if (left_ips[i] == right_ips[i]) continue;
      if (left_ips[i] < right_ips[i]) return -1;
      return  1;
   }
   // Common part is equal => compare lengths.
   if (left->n_ips_sel < right->n_ips_sel) return -1;
   if (left->n_ips_sel > right->n_ips_sel) return  1;
   return 0;
}

// If needed, build or refresh shared->ips_order_xecu
static void ensure_ips_order_xecu_valid(XT_shared* shared)
{
   UInt i;
   UInt n_xecu;

   if (shared->ips_order_xecu == NULL) {
      shared->ips_order_xecu = VG_(newXA)(shared->alloc_fn, shared->cc, 
                                          shared->free_fn, sizeof(Xecu));
      VG_(hintSizeXA)(shared->ips_order_xecu, VG_(sizeXA)(shared->xec));
      VG_(setCmpFnXA)(shared->ips_order_xecu, ips_order_cmp);
   }

   if (VG_(sizeXA)(shared->xec) == VG_(sizeXA)(shared->ips_order_xecu))
      return;

   n_xecu = VG_(sizeXA)(shared->xec);
   for (i = VG_(sizeXA)(shared->ips_order_xecu); i < n_xecu; i++)
      VG_(addToXA)(shared->ips_order_xecu, &i);
 
   xec_data_for_sort = shared->xec;
   VG_(sortXA)(shared->ips_order_xecu);
}

static void addRef_XT_shared (XT_shared* shared)
{
   shared->nrRef++;
}

static UWord release_XT_shared(XT_shared* shared)
{
   UWord nrRef;

   vg_assert(shared->nrRef > 0);
   nrRef = --shared->nrRef;
   if (nrRef == 0)
      delete_XT_shared(shared);
   return nrRef;
}

   
struct _XTree {
   Alloc_Fn_t alloc_fn;                /* alloc fn (nofail) */
   const HChar* cc;                    /* cost centre for alloc */
   Free_Fn_t free_fn;                  /* free fn */
   Word  dataSzB;   /* data size in bytes */
   XT_init_data_t init_data_fn;
   XT_add_data_t add_data_fn;
   XT_sub_data_t sub_data_fn;
   XT_filter_IPs_t filter_IPs_fn;

   XT_shared* shared;

   HChar* tmp_data; /* temporary buffer, to insert new elements. */
   XArray* data; /* of elements of size dataSzB */
};


XTree* VG_(XT_create) ( Alloc_Fn_t alloc_fn,
                        const HChar* cc,
                        Free_Fn_t free_fn,
                        Word dataSzB,
                        XT_init_data_t init_data_fn,
                        XT_add_data_t add_data_fn,
                        XT_sub_data_t sub_data_fn,
                        XT_filter_IPs_t filter_IPs_fn)
{
   XTree* xt;

   /* check user-supplied info .. */
   vg_assert(alloc_fn);
   vg_assert(free_fn);
   vg_assert(dataSzB >= 0);
   vg_assert(init_data_fn);
   vg_assert(add_data_fn);
   vg_assert(sub_data_fn);

   xt = alloc_fn(cc, sizeof(struct _XTree) );
   xt->alloc_fn  = alloc_fn;
   xt->cc        = cc;
   xt->free_fn   = free_fn;
   xt->dataSzB   = dataSzB;
   xt->init_data_fn = init_data_fn;
   xt->add_data_fn = add_data_fn;
   xt->sub_data_fn = sub_data_fn;
   xt->filter_IPs_fn = filter_IPs_fn;

   xt->shared = new_XT_shared(alloc_fn, cc, free_fn);
   addRef_XT_shared(xt->shared);
   xt->tmp_data = alloc_fn(cc, xt->dataSzB);
   xt->data =  VG_(newXA)(alloc_fn, cc, free_fn, dataSzB);

   return xt;
}

XTree* VG_(XT_snapshot)(XTree* xt)
{
   XTree* nxt;

   vg_assert(xt);

   nxt = xt->alloc_fn(xt->cc, sizeof(struct _XTree) );

   *nxt = *xt;
   addRef_XT_shared(nxt->shared);
   nxt->tmp_data = nxt->alloc_fn(nxt->cc, nxt->dataSzB);
   nxt->data = VG_(cloneXA)(nxt->cc, xt->data);

   return nxt;
}

void VG_(XT_delete) ( XTree* xt )
{
   vg_assert(xt);

   release_XT_shared(xt->shared);
   xt->free_fn(xt->tmp_data);
   VG_(deleteXA)(xt->data);
   xt->free_fn(xt);
}

static Xecu find_or_insert (XTree* xt, ExeContext* ec)
{

   const UInt d4ecu = VG_(get_ECU_from_ExeContext)(ec) / 4;
   XT_shared* shared = xt->shared;

   /* First grow the d4ecu2xecu array if needed. */
   if (d4ecu >= shared->d4ecu2xecu_sz) {
      UInt old_sz = shared->d4ecu2xecu_sz;
      UInt new_sz = (3 * d4ecu) / 2;

      if (new_sz < 1000)
         new_sz = 1000;
      shared->d4ecu2xecu = VG_(realloc)(xt->cc, shared->d4ecu2xecu,
                                        new_sz * sizeof(UInt));
      shared->d4ecu2xecu_sz = new_sz;
      for (UInt i = old_sz; i < new_sz; i++)
         shared->d4ecu2xecu[i] = NO_OFFSET;
   }

   if (shared->d4ecu2xecu[d4ecu] == NO_OFFSET) {
      xec xe;
     
      xe.ec = ec;
      if (xt->filter_IPs_fn == NULL) {
         xe.top = 0;
         xe.n_ips_sel = (UShort)VG_(get_ExeContext_n_ips)(xe.ec);
      } else {
         UInt top;
         UInt n_ips_sel = VG_(get_ExeContext_n_ips)(xe.ec);
         xt->filter_IPs_fn(VG_(get_ExeContext_StackTrace)(xe.ec), n_ips_sel,
                           &top, &n_ips_sel);
         xe.top = (UShort)top;
         xe.n_ips_sel = (UShort)n_ips_sel;
      }
      xt->init_data_fn(xt->tmp_data);
      VG_(addToXA)(shared->xec, &xe);
      shared->d4ecu2xecu[d4ecu] = (UInt)VG_(addToXA)(xt->data, xt->tmp_data);
   } 

   return shared->d4ecu2xecu[d4ecu];
}

Xecu VG_(XT_add_to_ec) (XTree* xt, ExeContext* ec, const void* value)
{
   Xecu xecu = find_or_insert(xt, ec);
   void* data = VG_(indexXA)(xt->data, xecu);

   xt->add_data_fn(data, value);
   return xecu;
}

Xecu VG_(XT_sub_from_ec) (XTree* xt, ExeContext* ec, const void* value)
{
   Xecu xecu = find_or_insert(xt, ec);
   void* data = VG_(indexXA)(xt->data, xecu);

   xt->sub_data_fn(data, value);
   return xecu;
}

void VG_(XT_add_to_xecu) (XTree* xt, Xecu xecu, const void* value)
{
   void* data = VG_(indexXA)(xt->data, xecu);
   xt->add_data_fn(data, value);
}

void VG_(XT_sub_from_xecu) (XTree* xt, Xecu xecu, const void* value)
{
   void* data = VG_(indexXA)(xt->data, xecu);
   xt->sub_data_fn(data, value);
}

UInt VG_(XT_n_ips_sel) (XTree* xt, Xecu xecu)
{
   xec* xe = (xec*)VG_(indexXA)(xt->shared->xec, xecu);
   return (UInt)xe->n_ips_sel;
}

ExeContext* VG_(XT_get_ec_from_xecu) (XTree* xt, Xecu xecu)
{
   xec* xe = (xec*)VG_(indexXA)(xt->shared->xec, xecu);
   return xe->ec;
}

static VgFile* xt_open (const HChar* outfilename)
{
   VgFile* fp;

   fp = VG_(fopen)(outfilename, VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC,
                   VKI_S_IRUSR|VKI_S_IWUSR|VKI_S_IRGRP|VKI_S_IROTH);
   if (fp == NULL) {
      VG_(message)(Vg_UserMsg,
                   "Error: can not open xtree output file `%s'\n",
                   outfilename);
   }
   return fp;
}

#define FP(format, args...) ({ VG_(fprintf)(fp, format, ##args); })

// Print "cmd:" line.
static void FP_cmd(VgFile* fp)
{
   UInt i;

   FP("cmd: ");
   FP("%s", VG_(args_the_exename));
   for (i = 0; i < VG_(sizeXA)(VG_(args_for_client)); i++) {
      HChar* arg = * (HChar**) VG_(indexXA)(VG_(args_for_client), i);
      FP(" %s", arg);
   }
   FP("\n");
}

/* ----------- Callgrind output ------------------------------------------- */

/* Output a callgrind format element in compressed format:
     "name=(pos)" or "name=(pos) value" (if value_new)
   or not compressed format: "name=value"
   VG_(clo_xtree_compress_strings) indicates if the compressed format is used.
   name is the format element (e.g. fl, fn, cfi, cfn, ...).
   pos is the value dictionary position, used for compressed format.
   value_new is True if this is the first usage of value. */
static void FP_pos_str(VgFile* fp, const HChar* name, UInt pos,
                       const HChar* value, Bool value_new)
{
   if (!VG_(clo_xtree_compress_strings))
      FP("%s=%s\n", name, value);
   else if (value_new)
      FP("%s=(%u) %s\n", name, pos, value);
   else
      FP("%s=(%u)\n", name, pos);
}

void VG_(XT_callgrind_print)
     (XTree* xt,
      const HChar* outfilename,
      const HChar* events,
      const HChar* (*img_value)(const void* value))
{
   UInt n_xecu;
   XT_shared* shared = xt->shared;
   VgFile* fp = xt_open(outfilename);
   DedupPoolAlloc* fnname_ddpa;
   DedupPoolAlloc* filename_ddpa;
   HChar* filename_buf = NULL;
   UInt filename_buf_size = 0;
   const HChar* filename_dir;
   const HChar* filename_name;

   if (fp == NULL)
      return;

   fnname_ddpa = VG_(newDedupPA)(16000, 1, xt->alloc_fn,
                                 "XT_callgrind_print.fn", xt->free_fn);
   filename_ddpa = VG_(newDedupPA)(16000, 1, xt->alloc_fn,
                                   "XT_callgrind_print.fl", xt->free_fn);

   FP("# callgrind format\n");
   FP("version: 1\n");
   FP("creator: xtree-1\n");
   FP("pid: %d\n", VG_(getpid)());
   FP_cmd(fp);

   /* Currently, we only need/support line positions. */
   FP("\npositions:%s\n", " line");

   /* Produce one "event:" line for each event, and the "events:" line. */
   {
      HChar strtok_events[VG_(strlen)(events)+1];
      HChar* e;
      HChar* ssaveptr;
      HChar* p;

      VG_(strcpy)(strtok_events, events);
      for (e = VG_(strtok_r)(strtok_events, ",", &ssaveptr); 
           e != NULL; 
           e = VG_(strtok_r)(NULL, ",", &ssaveptr))
         FP("event: %s\n", e);
      FP("events:");
      VG_(strcpy)(strtok_events, events);
      for (e = VG_(strtok_r)(strtok_events, ",", &ssaveptr); 
           e != NULL; 
           e = VG_(strtok_r)(NULL, ",", &ssaveptr)) {
         p = e;
         while (*p) {
            if (*p == ':')
               *p = 0;
            p++;
         }
         FP(" %s", e);
      }
      FP("\n");
   }
   xt->init_data_fn(xt->tmp_data); // to compute totals

   n_xecu = VG_(sizeXA)(xt->data);
   vg_assert (n_xecu <= VG_(sizeXA)(shared->xec));
   for (Xecu xecu = 0; xecu < n_xecu; xecu++) {
      xec* xe = (xec*)VG_(indexXA)(shared->xec, xecu);
      if (xe->n_ips_sel == 0)
         continue;

      const HChar* img = img_value(VG_(indexXA)(xt->data, xecu));
     
      // CALLED_FLF gets the Dir+Filename/Line number/Function name for ips[n]
      // in the variables called_filename/called_linenum/called_fnname.
      // The booleans called_filename_new/called_fnname_new are set to True
      // the first time the called_filename/called_fnname are encountered.
      // The called_filename_nr/called_fnname_nr are numbers identifying
      // the strings  called_filename/called_fnname.
#define CALLED_FLF(n)                                                   \
      if ((n) < 0                                                       \
          || !VG_(get_filename_linenum)(ep, ips[(n)],                   \
                                        &filename_name,                 \
                                        &filename_dir,                  \
                                        &called_linenum)) {             \
         filename_name = "UnknownFile???";                              \
         called_linenum = 0;                                            \
      }                                                                 \
      if ((n) < 0                                                       \
          || !VG_(get_fnname)(ep, ips[(n)], &called_fnname)) {          \
         called_fnname = "UnknownFn???";                                \
      }                                                                 \
      {                                                                 \
         UInt needed_size = VG_(strlen)(filename_dir) + 1               \
            + VG_(strlen)(filename_name) + 1;                           \
         if (filename_buf_size < needed_size) {                         \
            filename_buf_size = needed_size;                            \
            filename_buf = VG_(realloc)(xt->cc, filename_buf,           \
                                        filename_buf_size);             \
         }                                                              \
      }                                                                 \
      VG_(strcpy)(filename_buf, filename_dir);                          \
      if (filename_buf[0] != '\0') {                                    \
         VG_(strcat)(filename_buf, "/");                                \
      }                                                                 \
      VG_(strcat)(filename_buf, filename_name);                         \
      called_filename_nr = VG_(allocStrDedupPA)(filename_ddpa,          \
                                                filename_buf,           \
                                                &called_filename_new);  \
      called_filename = filename_buf;                                   \
      called_fnname_nr = VG_(allocStrDedupPA)(fnname_ddpa,              \
                                              called_fnname,            \
                                              &called_fnname_new);

      /* Instead of unknown fnname ???, CALLED_FLF could use instead:
         VG_(sprintf)(unknown_fn, "%p", (void*)ips[(n)]);
         but that creates a lot of (useless) nodes at least for
         valgrind self-hosting. */
      
      if (img) {
         const HChar* called_filename;
         UInt called_filename_nr;
         Bool called_filename_new; // True the first time we see this filename.
         const HChar* called_fnname;
         UInt called_fnname_nr;
         Bool called_fnname_new; // True the first time we see this fnname.
         UInt called_linenum;
         UInt prev_linenum;

         const Addr* ips = VG_(get_ExeContext_StackTrace)(xe->ec) + xe->top;
         const DiEpoch ep = VG_(get_ExeContext_epoch)(xe->ec);

         Int ips_idx = xe->n_ips_sel - 1;

         if (0) {
            VG_(printf)("entry img %s\n", img);
            VG_(pp_ExeContext)(xe->ec);
            VG_(printf)("\n");
         }
         xt->add_data_fn(xt->tmp_data, VG_(indexXA)(xt->data, xecu));
         CALLED_FLF(ips_idx);
         for (;
              ips_idx >= 0;
              ips_idx--) {
            FP_pos_str(fp, "fl", called_filename_nr,
                       called_filename, called_filename_new);
            FP_pos_str(fp, "fn", called_fnname_nr,
                       called_fnname, called_fnname_new);
            if (ips_idx == 0)
               FP("%u %s\n", called_linenum, img);
            else
               FP("%u\n", called_linenum); //no self cost.
            prev_linenum = called_linenum;
            if (ips_idx >= 1) {
               CALLED_FLF(ips_idx-1);
               FP_pos_str(fp, "cfi", called_filename_nr,
                          called_filename, called_filename_new);
               FP_pos_str(fp, "cfn", called_fnname_nr,
                          called_fnname, called_fnname_new);
               called_filename_new = False;
               called_fnname_new = False;
               /* Giving a call count of 0 allows kcachegrind to hide the calls
                  column. A call count of 1 means kcachegrind would show in the
                  calls column the nr of stacktrace containing this arc, which
                  is very confusing. So, the less bad is to give a 0 call
                  count. */
               FP("calls=0 %u\n", called_linenum);
               FP("%u %s\n", prev_linenum, img);
            }
         }
         FP("\n");
      }
   }
   /* callgrind format is not really fully supporting (yet?) execution trees:
      in an execution tree, self and inclusive costs are identical, and
      cannot be added together.
      If no totals: line is given, callgrind_annotate calculates the addition
      of all costs, and so gives a wrong totals.
      Giving a totals: line solves this, but the user must give the option
      --inclusive=yes (kind of hack) to have all the functions given
      in the output file. */
   FP("totals: %s\n", img_value(xt->tmp_data));
   VG_(fclose)(fp);
   VG_(deleteDedupPA)(fnname_ddpa);
   VG_(deleteDedupPA)(filename_ddpa);
   VG_(free)(filename_buf);
}


/* ----------- Massif output ---------------------------------------------- */

/* For Massif output, some functions from the execontext are not output, a.o.
   the allocation functions at the top of the stack and the functions below
   main. So, the StackTrace of the execontexts in the xtree must be filtered.
   Ms_Ec defines the subset of the stacktrace relevant for the report. */
typedef
   struct {
      StackTrace ips; // ips and n_ips provides the subset of the xtree ec
      UInt n_ips;     // to use for a massif report.

      SizeT report_value; // The value to report for this stack trace.
   } Ms_Ec;

/* Ms_Group defines (at a certain depth) a group of ec context that
   have the same IPs at the given depth, and have the same 'parent'.
   total is the sum of the values of all group elements.
   A Ms_Group can also represent a set of ec contexts that do not
   have the same IP, but that have each a total which is below the
   significant size. Such a group has a NULL ms_ec, a zero group_io.
   n_ec is the nr of insignificant ec that have been collected inside this
   insignificant group, and total is the sum of all non significant ec
   at the given depth. */
typedef
   struct {
      Ms_Ec* ms_ec; // The group consists in ms_ec[0 .. n_ec-1]
      Addr group_ip;
      UInt n_ec;
      SizeT total;
   } Ms_Group;

/* Compare 2 groups by total, to have bigger total first. */
static Int ms_group_revcmp_total(const void* vleft, const void* vright)
{
   const Ms_Group* left = (const Ms_Group*)vleft;
   const Ms_Group* right = (const Ms_Group*)vright;

   // First reverse compare total
   if (left->total > right->total) return -1;
   if (left->total < right->total) return  1;

   /* Equal size => compare IPs.
      This (somewhat?) helps to have deterministic test results.
      If this would change between platforms, then we should compare
      function names/filename/linenr */
   if (left->group_ip < right->group_ip) return -1;
   if (left->group_ip > right->group_ip) return  1;
   return 0;
}

/* Scan the addresses in ms_ec at the given depth.
   On return, 
      *groups points to an array of Ms_Group sorted by total.
      *n_groups is the nr of groups
   The caller is responsible to free the allocated group array. */
static void ms_make_groups (UInt depth, Ms_Ec* ms_ec, UInt n_ec, SizeT sig_sz,
                            UInt* n_groups, Ms_Group** groups)
{
   UInt i, g;
   Addr cur_group_ip = 0;

   *n_groups = 0;

   /* Handle special case somewhat more efficiently */
   if (n_ec == 0) {
      *groups = NULL;
      return;
   }

   /* Compute how many groups we have. */
   for (i = 0; i < n_ec; i++) {
      if (ms_ec[i].n_ips > depth
          && (*n_groups == 0 || cur_group_ip != ms_ec[i].ips[depth])) {
         (*n_groups)++;
         cur_group_ip = ms_ec[i].ips[depth];
      }
   }

   /* make the group array. */
   *groups = VG_(malloc)("ms_make_groups", *n_groups * sizeof(Ms_Group));
   i = 0;
   for (g = 0; g < *n_groups; g++) {
      while (ms_ec[i].n_ips <= depth)
         i++;
      cur_group_ip = ms_ec[i].ips[depth];
      (*groups)[g].group_ip = cur_group_ip;
      (*groups)[g].ms_ec = &ms_ec[i];
      (*groups)[g].n_ec = 1;
      (*groups)[g].total = ms_ec[i].report_value;
      i++;
      while (i < n_ec 
             && ms_ec[i].n_ips > depth
             && cur_group_ip == ms_ec[i].ips[depth]) {
         (*groups)[g].total += ms_ec[i].report_value;
         i++;
         (*groups)[g].n_ec++;
      }
   }

   /* Search for insignificant groups, collect them all together
      in the first insignificant group, and compact the group array. */
   {
      UInt insig1; // Position of first insignificant group.
      UInt n_insig = 0; // Nr of insignificant groups found.
      
      for (g = 0; g < *n_groups; g++) {
         if ((*groups)[g].total < sig_sz) {
            if (n_insig == 0) {
               // First insig group => transform it into the special group
               (*groups)[g].ms_ec = NULL;
               (*groups)[g].group_ip = 0;
               (*groups)[g].n_ec = 0;
               // start the sum of insig total as total
               insig1 = g;
            } else {
               // Add this insig group total into insig1 first group
               (*groups)[insig1].total += (*groups)[g].total;
            }
            n_insig++;
         } else {
            if (n_insig > 1)
               (*groups)[g - n_insig + 1] = (*groups)[g];
         }
      }
      if (n_insig > 0) {
         (*groups)[insig1].n_ec = n_insig;
         *n_groups -= n_insig - 1;
      }
      DMSG(1, "depth %u n_groups %u n_insig %u\n", depth, *n_groups, n_insig);
   }

   /* Sort on total size, bigger size first. */
   VG_(ssort)(*groups, *n_groups, sizeof(Ms_Group), ms_group_revcmp_total);
}

/* Output the given group (located in an xtree at the given depth).
   indent tells by how much to indent the information output for the group.
   indent can be bigger than depth when outputting a group that is made
   of one or more inlined calls: all inlined calls are output with the
   same depth but with one more indent for each inlined call.  */
static void ms_output_group (VgFile* fp, UInt depth, UInt indent,
                             Ms_Group* group, SizeT sig_sz,
                             double sig_pct_threshold)
{
   UInt i;
   Ms_Group* groups;
   UInt n_groups;

   // If this is an insignificant group, handle it specially
   if (group->ms_ec == NULL) {
      const HChar* s = ( 1 ==  group->n_ec? "," : "s, all" );
      vg_assert(group->group_ip == 0);
      FP("%*sn0: %lu in %u place%s below massif's threshold (%.2f%%)\n",
         (Int)(indent+1), "", group->total, group->n_ec, s, sig_pct_threshold);
      return;
   }

   // Normal group => output the group and its subgroups.
   ms_make_groups(depth+1, group->ms_ec, group->n_ec, sig_sz,
                  &n_groups, &groups);

   // FIXME JRS EPOCH 28 July 2017: HACK!  Is this correct?
   const DiEpoch cur_ep = VG_(current_DiEpoch)();
   // // FIXME PW EPOCH : No, the above is not correct.
   // Xtree Massif output regroups execontext in the layout of a 'tree'.
   // So, possibly, the same IP address value can be in 2 different ec, but
   // the epoch to symbolise this address must be retrieved from the ec it
   // originates from.
   // So, to fix this, it is not enough to make a group based on identical
   // IP addr value, one must also find the di used to symbolise this address,
   // A group will then be defined as 'same IP and same di'.
   // Fix not trivial to do, so for the moment, --keep-debuginfo=yes will
   // have no impact on xtree massif output.

   Addr cur_ip = group->ms_ec->ips[depth];

   InlIPCursor *iipc = VG_(new_IIPC)(cur_ep, cur_ip);

   while (True) {
      const HChar* buf = VG_(describe_IP)(cur_ep, cur_ip, iipc);
      Bool is_inlined = VG_(next_IIPC)(iipc);

      FP("%*s" "n%u: %lu %s\n",
         (Int)(indent + 1), "",
         is_inlined ? 1 : n_groups, // Inlined frames always have one child.
         group->total,
         buf);

      if (!is_inlined) {
         break;
      }

      indent++;
   }

   VG_(delete_IIPC)(iipc);

   /* Output sub groups of this group. */
   for (i = 0; i < n_groups; i++)
      ms_output_group(fp, depth+1, indent+1, &groups[i], sig_sz,
                      sig_pct_threshold);

   VG_(free)(groups);
}

/* Allocate and build an array of Ms_Ec sorted by addresses in the
   Ms_Ec StackTrace. */
static void prepare_ms_ec (XTree* xt,
                           ULong (*report_value)(const void* value),
                           ULong* top_total, Ms_Ec** vms_ec, UInt* vn_ec)
{
   XT_shared* shared = xt->shared;
   const UInt n_xecu = VG_(sizeXA)(shared->xec);
   const UInt n_data_xecu = VG_(sizeXA)(xt->data);
   Ms_Ec* ms_ec = VG_(malloc)("XT_massif_print.ms_ec", n_xecu * sizeof(Ms_Ec));
   UInt n_xecu_sel = 0; // Nr of xecu that are selected for output.

   vg_assert(n_data_xecu <= n_xecu);

   // Ensure we have in shared->ips_order_xecu our xecu sorted by StackTrace.
   ensure_ips_order_xecu_valid(shared);

   *top_total = 0;
   DMSG(1, "iteration %u\n", n_xecu);
   for (UInt i = 0; i < n_xecu; i++) {
      Xecu xecu = *(Xecu*)VG_(indexXA)(shared->ips_order_xecu, i);
      xec* xe = (xec*)VG_(indexXA)(shared->xec, xecu);

      if (xecu >= n_data_xecu)
         continue; // No data for this xecu in xt->data.
      ms_ec[n_xecu_sel].n_ips = xe->n_ips_sel;
      if (ms_ec[n_xecu_sel].n_ips == 0)
         continue;
            
      ms_ec[n_xecu_sel].ips = VG_(get_ExeContext_StackTrace)(xe->ec) + xe->top;
      ms_ec[n_xecu_sel].report_value
         = (*report_value)(VG_(indexXA)(xt->data, xecu));
      *top_total += ms_ec[n_xecu_sel].report_value;

      n_xecu_sel++;
   }
   vg_assert(n_xecu_sel <= n_xecu);
      
   *vms_ec = ms_ec;
   *vn_ec = n_xecu_sel;
}

MsFile* VG_(XT_massif_open)
     (const HChar* outfilename,
      const HChar* desc,
      const XArray* desc_args,
      const HChar* time_unit)
{
   UInt i;
   VgFile* fp = xt_open(outfilename);
   
   if (fp == NULL)
      return NULL; // xt_open reported the error.
   
   /* ------ file header ------------------------------- */
   FP("desc:");
   if (desc)
      FP(" %s", desc);
   i = 0;
   if (desc_args) {
      for (i = 0; i < VG_(sizeXA)(desc_args); i++) {
         HChar* arg = *(HChar**)VG_(indexXA)(desc_args, i);
         FP(" %s", arg);
      }
   }
   if (0 == i && desc == NULL) FP(" (none)");
   FP("\n");

   FP_cmd(fp);

   FP("time_unit: %s\n", time_unit);

   return fp;
}

void VG_(XT_massif_close)(MsFile* fp)
{
   if (fp == NULL)
      return; // Error should have been reported by  VG_(XT_massif_open)

   VG_(fclose)(fp);
}

void VG_(XT_massif_print) 
     (MsFile* fp,
      XTree* xt,
      const Massif_Header* header,
      ULong (*report_value)(const void* value))
{
   UInt i;

   if (fp == NULL)
      return; // Normally  VG_(XT_massif_open) already reported an error.

   /* Compute/prepare Snapshot totals/data/... */
   ULong top_total;

   /* Following variables only used for detailed snapshot. */
   UInt n_ec = 0;
   Ms_Ec* ms_ec = NULL;
   const HChar* kind = 
      header->detailed ? (header->peak ? "peak" : "detailed") : "empty";

   DMSG(1, "XT_massif_print %s\n", kind);
   if (header->detailed) {
      /* Prepare the Ms_Ec sorted array of stacktraces and the groups
         at level 0. */
      prepare_ms_ec(xt, report_value, &top_total, &ms_ec, &n_ec);
      DMSG(1, "XT_print_massif ms_ec n_ec %u\n", n_ec);
   } else if (xt == NULL) {
      /* Non detailed, no xt => use the sz provided in the header. */
      top_total = header->sz_B;
   } else {
      /* For non detailed snapshot, compute total directly from the xec. */
      const XT_shared* shared = xt->shared;
      const UInt n_xecu = VG_(sizeXA)(xt->data);
      top_total = 0;
      
      for (UInt xecu = 0; xecu < n_xecu; xecu++) {
         xec* xe = (xec*)VG_(indexXA)(shared->xec, xecu);
         if (xe->n_ips_sel == 0)
            continue;
         top_total += (*report_value)(VG_(indexXA)(xt->data, xecu));
      }
   }

   /* ------ snapshot header --------------------------- */
   FP("#-----------\n");
   FP("snapshot=%d\n", header->snapshot_n);
   FP("#-----------\n");
   FP("time=%lld\n", header->time);
   
   FP("mem_heap_B=%llu\n", top_total); // without extra_B and without stacks_B
   FP("mem_heap_extra_B=%llu\n", header->extra_B);
   FP("mem_stacks_B=%llu\n", header->stacks_B);
   FP("heap_tree=%s\n", kind);

   /* ------ detailed snapshot data ----------------------------- */
   if (header->detailed) {
      UInt n_groups;
      Ms_Group* groups;

      ULong sig_sz;
      // Work out how big a child must be to be significant.  If the current
      // top_total is zero, then we set it to 1, which means everything will be
      // judged insignificant -- this is sensible, as there's no point showing
      // any detail for this case.  Unless they used threshold=0, in which
      // case we show them everything because that's what they asked for.
      //
      // Nb: We do this once now, rather than once per child, because if we do
      // that the cost of all the divisions adds up to something significant.
      if (0 == top_total && 0 != header->sig_threshold)
         sig_sz = 1;
      else
         sig_sz = ((top_total + header->extra_B + header->stacks_B) 
                   * header->sig_threshold) / 100;

      /* Produce the groups at depth 0 */
      DMSG(1, "XT_massif_print producing depth 0 groups\n");
      ms_make_groups(0, ms_ec, n_ec, sig_sz, &n_groups, &groups);

      /* Output the top node. */
      FP("n%u: %llu %s\n", n_groups, top_total, header->top_node_desc);

      /* Output depth 0 groups. */
      DMSG(1, "XT_massif_print outputting %u depth 0 groups\n", n_groups);
      for (i = 0; i < n_groups; i++)
         ms_output_group(fp, 0, 0, &groups[i], sig_sz, header->sig_threshold);

      VG_(free)(groups);
      VG_(free)(ms_ec);
   }
}

Int VG_(XT_offset_main_or_below_main)(DiEpoch ep, Addr* ips, Int n_ips)
{
   /* Search for main or below main function.
      To limit the nr of ips to examine, we maintain the deepest
      offset where main was found, and we first search main
      from there.
      If no main is found, we will then do a search for main or
      below main function till the top. */
   static Int deepest_main = 0;
   Vg_FnNameKind kind = Vg_FnNameNormal;
   Int mbm = n_ips - 1; // Position of deepest main or below main.
   Vg_FnNameKind mbmkind = Vg_FnNameNormal;
   Int i;

   for (i = n_ips - 1 - deepest_main;
        i < n_ips;
        i++) {
      mbmkind = VG_(get_fnname_kind_from_IP)(ep, ips[i]);
      if (mbmkind != Vg_FnNameNormal) {
         mbm = i;
         break;
      }
   }

   /* Search for main or below main function till top. */
   for (i = mbm - 1;
        i >= 0 && mbmkind != Vg_FnNameMain;
        i--) {
      kind = VG_(get_fnname_kind_from_IP)(ep, ips[i]);
      if (kind != Vg_FnNameNormal) {
         mbm = i;
         mbmkind = kind;
      }
   }
   if (Vg_FnNameMain == mbmkind || Vg_FnNameBelowMain == mbmkind) {
      if (mbmkind == Vg_FnNameMain && (n_ips - 1 - mbm) > deepest_main)
         deepest_main = n_ips - 1 - mbm;
      return mbm;
   } else
      return n_ips-1;
}

void VG_(XT_filter_1top_and_maybe_below_main)
     (Addr* ips, Int n_ips,
      UInt* top, UInt* n_ips_sel)
{
   Int mbm;

   *n_ips_sel = n_ips;
   if (n_ips == 0) {
      *top = 0;
      return;
   }

   /* Filter top function. */
   *top = 1;

   if (VG_(clo_show_below_main))
      mbm = n_ips - 1;
   else {
      // FIXME PW EPOCH : use the real ips epoch
      const DiEpoch cur_ep = VG_(current_DiEpoch)();
      mbm = VG_(XT_offset_main_or_below_main)(cur_ep, ips, n_ips);
   }

   *n_ips_sel = mbm - *top + 1;
}

void VG_(XT_filter_maybe_below_main)
     (Addr* ips, Int n_ips,
      UInt* top, UInt* n_ips_sel)
{
   Int mbm;

   *n_ips_sel = n_ips;
   *top = 0;
   if (n_ips == 0)
      return;

   if (VG_(clo_show_below_main))
      mbm = n_ips - 1;
   else {
      // FIXME PW EPOCH : use the real ips epoch
      const DiEpoch cur_ep = VG_(current_DiEpoch)();
      mbm = VG_(XT_offset_main_or_below_main)(cur_ep, ips, n_ips);
   }
   *n_ips_sel = mbm - *top + 1;
}

/*--------------------------------------------------------------------*/
/*--- end                                                m_xtree.c ---*/
/*--------------------------------------------------------------------*/
