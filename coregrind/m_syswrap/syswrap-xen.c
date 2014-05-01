
/*--------------------------------------------------------------------*/
/*--- Xen Hypercalls                                 syswrap-xen.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2012 Citrix Systems
      ian.campbell@citrix.com

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

#include "pub_core_basics.h"
#include "pub_core_vki.h"

#if defined(ENABLE_XEN)

#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"   // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuginfo.h"    // VG_(di_notify_*)
#include "pub_core_transtab.h"     // VG_(discard_translations)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-xen.h"

#include <inttypes.h>

#define PRE(name) static DEFN_PRE_TEMPLATE(xen, name)
#define POST(name) static DEFN_POST_TEMPLATE(xen, name)

static void bad_subop ( ThreadId              tid,
                        SyscallArgLayout*     layout,
                        /*MOD*/SyscallArgs*   args,
                        /*OUT*/SyscallStatus* status,
                        /*OUT*/UWord*         flags,
                        const HChar*           hypercall,
                        UWord                 subop)
{
   VG_(dmsg)("WARNING: unhandled %s subop: %ld\n",
             hypercall, subop);
   if (VG_(clo_verbosity) > 1) {
      VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
   }
   VG_(dmsg)("You may be able to write your own handler.\n");
   VG_(dmsg)("Read the file README_MISSING_SYSCALL_OR_IOCTL.\n");
   VG_(dmsg)("Nevertheless we consider this a bug.  Please report\n");
   VG_(dmsg)("it at http://valgrind.org/support/bug_reports.html &\n");
   VG_(dmsg)("http://wiki.xen.org/wiki/Reporting_Bugs_against_Xen.\n");

   SET_STATUS_Failure(VKI_ENOSYS);
}

PRE(memory_op)
{
   PRINT("__HYPERVISOR_memory_op ( %ld, %lx )", ARG1, ARG2);

   switch (ARG1) {

   case VKI_XENMEM_maximum_ram_page:
       /* No inputs */
       break;

   case VKI_XENMEM_maximum_gpfn:
       PRE_MEM_READ("XENMEM_maximum_gpfn domid",
                    (Addr)ARG2, sizeof(vki_xen_domid_t));
       break;

   case VKI_XENMEM_machphys_mfn_list: {
       struct vki_xen_machphys_mfn_list *arg =
           (struct vki_xen_machphys_mfn_list *)ARG2;
       PRE_MEM_READ("XENMEM_machphys_mfn_list max_extents",
                    (Addr)&arg->max_extents, sizeof(arg->max_extents));
       PRE_MEM_READ("XENMEM_machphys_mfn_list extent_start",
                    (Addr)&arg->extent_start, sizeof(arg->extent_start));
       break;
   }

   case VKI_XENMEM_set_memory_map: {
      struct vki_xen_foreign_memory_map *arg =
	      (struct vki_xen_foreign_memory_map *)ARG2;
      PRE_MEM_READ("XENMEM_set_memory_map domid",
                   (Addr)&arg->domid, sizeof(arg->domid));
      PRE_MEM_READ("XENMEM_set_memory_map map",
                   (Addr)&arg->map, sizeof(arg->map));
      break;
   }
   case VKI_XENMEM_increase_reservation:
   case VKI_XENMEM_decrease_reservation:
   case VKI_XENMEM_populate_physmap:
   case VKI_XENMEM_claim_pages: {
      struct xen_memory_reservation *memory_reservation =
         (struct xen_memory_reservation *)ARG2;
      const HChar *which;

      switch (ARG1) {
      case VKI_XENMEM_increase_reservation:
         which = "XENMEM_increase_reservation";
         break;
      case VKI_XENMEM_decrease_reservation:
         which = "XENMEM_decrease_reservation";
         PRE_MEM_READ(which,
                      (Addr)memory_reservation->extent_start.p,
                      sizeof(vki_xen_pfn_t) * memory_reservation->nr_extents);
	 break;
      case VKI_XENMEM_populate_physmap:
         which = "XENMEM_populate_physmap";
         PRE_MEM_READ(which,
                      (Addr)memory_reservation->extent_start.p,
                      sizeof(vki_xen_pfn_t) * memory_reservation->nr_extents);
         break;
      case VKI_XENMEM_claim_pages:
         which = "XENMEM_claim_pages";
         break;
      default:
         which = "XENMEM_unknown";
         break;
      }

      PRE_MEM_READ(which,
                   (Addr)&memory_reservation->extent_start,
                   sizeof(memory_reservation->extent_start));
      PRE_MEM_READ(which,
                   (Addr)&memory_reservation->nr_extents,
                   sizeof(memory_reservation->nr_extents));
      PRE_MEM_READ(which,
                   (Addr)&memory_reservation->extent_order,
                   sizeof(memory_reservation->extent_order));
      PRE_MEM_READ(which,
                   (Addr)&memory_reservation->mem_flags,
                   sizeof(memory_reservation->mem_flags));
      PRE_MEM_READ(which,
                   (Addr)&memory_reservation->domid,
                   sizeof(memory_reservation->domid));
      break;
   }

   case VKI_XENMEM_add_to_physmap: {
       struct vki_xen_add_to_physmap *arg =
           (struct vki_xen_add_to_physmap *)ARG2;
       PRE_MEM_READ("XENMEM_add_to_physmap domid",
                    (Addr)&arg->domid, sizeof(arg->domid));
       PRE_MEM_READ("XENMEM_add_to_physmap size",
                    (Addr)&arg->size, sizeof(arg->size));
       PRE_MEM_READ("XENMEM_add_to_physmap space",
                    (Addr)&arg->space, sizeof(arg->space));
       PRE_MEM_READ("XENMEM_add_to_physmap idx",
                    (Addr)&arg->idx, sizeof(arg->idx));
       PRE_MEM_READ("XENMEM_add_to_physmap gpfn",
                    (Addr)&arg->gpfn, sizeof(arg->gpfn));
       break;
   };

   case VKI_XENMEM_remove_from_physmap: {
       struct vki_xen_remove_from_physmap *arg =
           (struct vki_xen_remove_from_physmap *)ARG2;
       PRE_MEM_READ("XENMEM_remove_from_physmap domid",
                    (Addr)&arg->domid, sizeof(arg->domid));
       PRE_MEM_READ("XENMEM_remove_from_physmap gpfn",
                    (Addr)&arg->gpfn, sizeof(arg->gpfn));
   }

   case VKI_XENMEM_get_sharing_freed_pages:
   case VKI_XENMEM_get_sharing_shared_pages:
      break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_memory_op", ARG1);
      break;
   }
}

PRE(mmuext_op)
{
   struct vki_xen_mmuext_op *ops = (struct vki_xen_mmuext_op *)ARG1;
   unsigned int i, nr = ARG2;

   for (i=0; i<nr; i++) {
      struct vki_xen_mmuext_op *op = ops + i;
      PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP cmd",
                   (Addr)&op->cmd, sizeof(op->cmd));
      switch(op->cmd) {
      case VKI_XEN_MMUEXT_PIN_L1_TABLE:
      case VKI_XEN_MMUEXT_PIN_L2_TABLE:
      case VKI_XEN_MMUEXT_PIN_L3_TABLE:
      case VKI_XEN_MMUEXT_PIN_L4_TABLE:
      case VKI_XEN_MMUEXT_UNPIN_TABLE:
      case VKI_XEN_MMUEXT_NEW_BASEPTR:
      case VKI_XEN_MMUEXT_CLEAR_PAGE:
      case VKI_XEN_MMUEXT_COPY_PAGE:
      case VKI_XEN_MMUEXT_MARK_SUPER:
      case VKI_XEN_MMUEXT_UNMARK_SUPER:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg1.mfn",
                      (Addr)&op->arg1.mfn,
                      sizeof(op->arg1.mfn));
         break;

      case VKI_XEN_MMUEXT_INVLPG_LOCAL:
      case VKI_XEN_MMUEXT_INVLPG_ALL:
      case VKI_XEN_MMUEXT_SET_LDT:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg1.mfn",
                      (Addr)&op->arg1.linear_addr,
                      sizeof(op->arg1.linear_addr));
         break;

      case VKI_XEN_MMUEXT_TLB_FLUSH_LOCAL:
      case VKI_XEN_MMUEXT_TLB_FLUSH_MULTI:
      case VKI_XEN_MMUEXT_INVLPG_MULTI:
      case VKI_XEN_MMUEXT_TLB_FLUSH_ALL:
      case VKI_XEN_MMUEXT_FLUSH_CACHE:
      case VKI_XEN_MMUEXT_NEW_USER_BASEPTR:
      case VKI_XEN_MMUEXT_FLUSH_CACHE_GLOBAL:
         /* None */
         break;
      }

      switch(op->cmd) {
      case VKI_XEN_MMUEXT_SET_LDT:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg2.nr_ents",
                      (Addr)&op->arg2.nr_ents,
                      sizeof(op->arg2.nr_ents));
         break;

      case VKI_XEN_MMUEXT_TLB_FLUSH_MULTI:
      case VKI_XEN_MMUEXT_INVLPG_MULTI:
         /* How many??? */
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg2.vcpumask",
                      (Addr)&op->arg2.vcpumask,
                      sizeof(op->arg2.vcpumask));
         break;

      case VKI_XEN_MMUEXT_COPY_PAGE:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg2.src_mfn",
                      (Addr)&op->arg2.src_mfn,
                      sizeof(op->arg2.src_mfn));
         break;

      case VKI_XEN_MMUEXT_PIN_L1_TABLE:
      case VKI_XEN_MMUEXT_PIN_L2_TABLE:
      case VKI_XEN_MMUEXT_PIN_L3_TABLE:
      case VKI_XEN_MMUEXT_PIN_L4_TABLE:
      case VKI_XEN_MMUEXT_UNPIN_TABLE:
      case VKI_XEN_MMUEXT_NEW_BASEPTR:
      case VKI_XEN_MMUEXT_TLB_FLUSH_LOCAL:
      case VKI_XEN_MMUEXT_INVLPG_LOCAL:
      case VKI_XEN_MMUEXT_TLB_FLUSH_ALL:
      case VKI_XEN_MMUEXT_INVLPG_ALL:
      case VKI_XEN_MMUEXT_FLUSH_CACHE:
      case VKI_XEN_MMUEXT_NEW_USER_BASEPTR:
      case VKI_XEN_MMUEXT_CLEAR_PAGE:
      case VKI_XEN_MMUEXT_FLUSH_CACHE_GLOBAL:
      case VKI_XEN_MMUEXT_MARK_SUPER:
      case VKI_XEN_MMUEXT_UNMARK_SUPER:
         /* None */
         break;
      }
   }
}

static void pre_evtchn_op(ThreadId tid,
                          SyscallArgLayout*     layout,
                          /*MOD*/SyscallArgs*   arrghs,
                          /*OUT*/SyscallStatus* status,
                          /*OUT*/UWord*         flags,
                          __vki_u32 cmd, void *arg, int compat)
{
   PRINT("__HYPERVISOR_event_channel_op%s ( %d, %p )",
         compat ? "_compat" : "", cmd, arg);

   switch (cmd) {
   case VKI_XEN_EVTCHNOP_alloc_unbound: {
      struct vki_xen_evtchn_alloc_unbound *alloc_unbound = arg;
      PRE_MEM_READ("EVTCHNOP_alloc_unbound dom",
                   (Addr)&alloc_unbound->dom, sizeof(alloc_unbound->dom));
      PRE_MEM_READ("EVTCHNOP_alloc_unbound remote_dom",
                   (Addr)&alloc_unbound->remote_dom,
                   sizeof(alloc_unbound->remote_dom));
      break;
   }
   default:
      if ( compat )
         bad_subop(tid, layout, arrghs, status, flags,
                   "__HYPERVISOR_event_channel_op_compat", cmd);
      else
         bad_subop(tid, layout, arrghs, status, flags,
                   "__HYPERVISOR_event_channel_op", cmd);
      break;
   }
}

PRE(evtchn_op)
{
   pre_evtchn_op(tid, layout, arrghs, status, flags,
                 ARG1, (void *)ARG2, 0);
}

PRE(evtchn_op_compat)
{
   struct vki_xen_evtchn_op *evtchn = (struct vki_xen_evtchn_op *)ARG1;
   PRE_MEM_READ("__HYPERVISOR_event_channel_op_compat",
                ARG1, sizeof(*evtchn));

   pre_evtchn_op(tid, layout, arrghs, status, flags,
                 evtchn->cmd, &evtchn->u, 1);
}

PRE(xen_version)
{
   PRINT("__HYPERVISOR_xen_version ( %ld, %lx )", ARG1, ARG2);

   switch (ARG1) {
   case VKI_XENVER_version:
   case VKI_XENVER_extraversion:
   case VKI_XENVER_compile_info:
   case VKI_XENVER_capabilities:
   case VKI_XENVER_changeset:
   case VKI_XENVER_platform_parameters:
   case VKI_XENVER_get_features:
   case VKI_XENVER_pagesize:
   case VKI_XENVER_guest_handle:
   case VKI_XENVER_commandline:
      /* No inputs */
      break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_xen_version", ARG1);
      break;
   }
}

PRE(grant_table_op)
{
   PRINT("__HYPERVISOR_grant_table_op ( %ld, 0x%lx, %ld )", ARG1, ARG2, ARG3);
   switch (ARG1) {
   case VKI_XEN_GNTTABOP_setup_table: {
      struct vki_xen_gnttab_setup_table *gst =
	      (struct vki_xen_gnttab_setup_table*)ARG2;
      PRE_MEM_READ("VKI_XEN_GNTTABOP_setup_table dom",
		   (Addr)&gst->dom, sizeof(gst->dom));
      PRE_MEM_READ("VKI_XEN_GNTTABOP_setup_table nr_frames",
                   (Addr)&gst->nr_frames, sizeof(gst->nr_frames));
      break;
   }
   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_grant_table_op", ARG1);
      break;
   }
}

PRE(sysctl) {
   struct vki_xen_sysctl *sysctl = (struct vki_xen_sysctl *)ARG1;

   PRINT("__HYPERVISOR_sysctl ( %d )", sysctl->cmd);

   /*
    * Common part of xen_sysctl:
    *    uint32_t cmd;
    *    uint32_t interface_version;
    */
   PRE_MEM_READ("__HYPERVISOR_sysctl", ARG1,
                sizeof(vki_uint32_t) + sizeof(vki_uint32_t));

   if (!sysctl)
      return;

   switch (sysctl->interface_version)
   {
   case 0x00000008:
   case 0x00000009:
   case 0x0000000a:
	   break;
   default:
      VG_(dmsg)("WARNING: sysctl version %"PRIx32" not supported\n",
                sysctl->interface_version);
      if (VG_(clo_verbosity) > 1) {
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      }
      VG_(dmsg)("You may be able to write your own handler.\n");
      VG_(dmsg)("Read the file README_MISSING_SYSCALL_OR_IOCTL.\n");
      VG_(dmsg)("Nevertheless we consider this a bug.  Please report\n");
      VG_(dmsg)("it at http://valgrind.org/support/bug_reports.html &\n");
      VG_(dmsg)("http://wiki.xen.org/wiki/Reporting_Bugs_against_Xen.\n");

      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

#define __PRE_XEN_SYSCTL_READ(_sysctl, _union, _field)			\
      PRE_MEM_READ("XEN_SYSCTL_" #_sysctl " u." #_union "." #_field,	\
                   (Addr)&sysctl->u._union._field,			\
                   sizeof(sysctl->u._union._field))
#define PRE_XEN_SYSCTL_READ(_sysctl, _field) \
      __PRE_XEN_SYSCTL_READ(_sysctl, _sysctl, _field)

   switch (sysctl->cmd) {
   case VKI_XEN_SYSCTL_readconsole:
       /* These are all unconditionally read */
       PRE_XEN_SYSCTL_READ(readconsole, clear);
       PRE_XEN_SYSCTL_READ(readconsole, incremental);
       PRE_XEN_SYSCTL_READ(readconsole, buffer);
       PRE_XEN_SYSCTL_READ(readconsole, count);

       /* 'index' only read if 'incremental' is nonzero */
       if (sysctl->u.readconsole.incremental)
           PRE_XEN_SYSCTL_READ(readconsole, index);
       break;

   case VKI_XEN_SYSCTL_getdomaininfolist:
      switch (sysctl->interface_version)
      {
      case 0x00000008:
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_00000008, first_domain);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_00000008, max_domains);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_00000008, buffer);
	 break;
      case 0x00000009:
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_00000009, first_domain);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_00000009, max_domains);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_00000009, buffer);
	 break;
      case 0x0000000a:
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_0000000a, first_domain);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_0000000a, max_domains);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_0000000a, buffer);
	 break;
      default:
          VG_(dmsg)("WARNING: XEN_SYSCTL_getdomaininfolist for sysctl version "
                    "%"PRIx32" not implemented yet\n",
                    sysctl->interface_version);
          SET_STATUS_Failure(VKI_EINVAL);
          return;
      }
      break;

   case VKI_XEN_SYSCTL_debug_keys:
       PRE_XEN_SYSCTL_READ(debug_keys, keys);
       PRE_XEN_SYSCTL_READ(debug_keys, nr_keys);
       PRE_MEM_READ("XEN_SYSCTL_debug_keys *keys",
                    (Addr)sysctl->u.debug_keys.keys.p,
                    sysctl->u.debug_keys.nr_keys * sizeof(char));
       break;

   case VKI_XEN_SYSCTL_sched_id:
       /* No inputs */
       break;

   case VKI_XEN_SYSCTL_cpupool_op:
      PRE_XEN_SYSCTL_READ(cpupool_op, op);

      switch(sysctl->u.cpupool_op.op) {
      case VKI_XEN_SYSCTL_CPUPOOL_OP_CREATE:
      case VKI_XEN_SYSCTL_CPUPOOL_OP_DESTROY:
      case VKI_XEN_SYSCTL_CPUPOOL_OP_INFO:
      case VKI_XEN_SYSCTL_CPUPOOL_OP_ADDCPU:
      case VKI_XEN_SYSCTL_CPUPOOL_OP_RMCPU:
      case VKI_XEN_SYSCTL_CPUPOOL_OP_MOVEDOMAIN:
         PRE_XEN_SYSCTL_READ(cpupool_op, cpupool_id);
      }

      if (sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_CREATE)
         PRE_XEN_SYSCTL_READ(cpupool_op, sched_id);

      if (sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_MOVEDOMAIN)
         PRE_XEN_SYSCTL_READ(cpupool_op, domid);

      if (sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_ADDCPU ||
          sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_RMCPU)
         PRE_XEN_SYSCTL_READ(cpupool_op, cpu);

      break;

   case VKI_XEN_SYSCTL_physinfo:
      /* No input params */
      break;

   case VKI_XEN_SYSCTL_topologyinfo:
      PRE_XEN_SYSCTL_READ(topologyinfo, max_cpu_index);
      PRE_XEN_SYSCTL_READ(topologyinfo, cpu_to_core);
      PRE_XEN_SYSCTL_READ(topologyinfo, cpu_to_socket);
      PRE_XEN_SYSCTL_READ(topologyinfo, cpu_to_node);
      break;

   case VKI_XEN_SYSCTL_numainfo:
      PRE_XEN_SYSCTL_READ(numainfo, max_node_index);
      PRE_XEN_SYSCTL_READ(numainfo, node_to_memsize);
      PRE_XEN_SYSCTL_READ(numainfo, node_to_memfree);
      PRE_XEN_SYSCTL_READ(numainfo, node_to_node_distance);
      break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_sysctl", sysctl->cmd);
      break;
   }
#undef PRE_XEN_SYSCTL_READ
#undef __PRE_XEN_SYSCTL_READ
}

PRE(domctl)
{
   struct vki_xen_domctl *domctl = (struct vki_xen_domctl *)ARG1;

   PRINT("__HYPERVISOR_domctl ( %d ) on dom%d", domctl->cmd, domctl->domain);

   /*
    * Common part of xen_domctl:
    *    vki_uint32_t cmd;
    *    vki_uint32_t interface_version;
    *    vki_xen_domid_t  domain;
    */
   PRE_MEM_READ("__HYPERVISOR_domctl", ARG1,
                sizeof(vki_uint32_t) + sizeof(vki_uint32_t)
		+ sizeof(vki_xen_domid_t));

   if (!domctl)
      return;

   switch (domctl->interface_version)
   {
   case 0x00000007:
   case 0x00000008:
   case 0x00000009:
	   break;
   default:
      VG_(dmsg)("WARNING: domctl version %"PRIx32" not supported\n",
                domctl->interface_version);
      if (VG_(clo_verbosity) > 1) {
         VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
      }
      VG_(dmsg)("You may be able to write your own handler.\n");
      VG_(dmsg)("Read the file README_MISSING_SYSCALL_OR_IOCTL.\n");
      VG_(dmsg)("Nevertheless we consider this a bug.  Please report\n");
      VG_(dmsg)("it at http://valgrind.org/support/bug_reports.html &\n");
      VG_(dmsg)("http://wiki.xen.org/wiki/Reporting_Bugs_against_Xen.\n");

      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

#define __PRE_XEN_DOMCTL_READ(_domctl, _union, _field)			\
      PRE_MEM_READ("XEN_DOMCTL_" #_domctl " u." #_union "." #_field,	\
                   (Addr)&domctl->u._union._field,			\
                   sizeof(domctl->u._union._field))
#define PRE_XEN_DOMCTL_READ(_domctl, _field) \
      __PRE_XEN_DOMCTL_READ(_domctl, _domctl, _field)

   switch (domctl->cmd) {
   case VKI_XEN_DOMCTL_destroydomain:
   case VKI_XEN_DOMCTL_pausedomain:
   case VKI_XEN_DOMCTL_max_vcpus:
   case VKI_XEN_DOMCTL_get_address_size:
   case VKI_XEN_DOMCTL_gettscinfo:
   case VKI_XEN_DOMCTL_getdomaininfo:
   case VKI_XEN_DOMCTL_unpausedomain:
   case VKI_XEN_DOMCTL_resumedomain:
      /* No input fields. */
      break;

   case VKI_XEN_DOMCTL_createdomain:
      PRE_XEN_DOMCTL_READ(createdomain, ssidref);
      PRE_XEN_DOMCTL_READ(createdomain, handle);
      PRE_XEN_DOMCTL_READ(createdomain, flags);
      break;

   case VKI_XEN_DOMCTL_gethvmcontext:
       /* Xen unconditionally reads the 'buffer' pointer */
       __PRE_XEN_DOMCTL_READ(gethvmcontext, hvmcontext, buffer);
       /* Xen only consumes 'size' if 'buffer' is non NULL. A NULL
        * buffer is a request for the required size. */
       if ( domctl->u.hvmcontext.buffer.p )
           __PRE_XEN_DOMCTL_READ(gethvmcontext, hvmcontext, size);
       break;

   case VKI_XEN_DOMCTL_sethvmcontext:
       __PRE_XEN_DOMCTL_READ(sethvmcontext, hvmcontext, size);
       __PRE_XEN_DOMCTL_READ(sethvmcontext, hvmcontext, buffer);
       PRE_MEM_READ("XEN_DOMCTL_sethvmcontext *buffer",
                    (Addr)domctl->u.hvmcontext.buffer.p,
                    domctl->u.hvmcontext.size);
       break;

   case VKI_XEN_DOMCTL_max_mem:
      PRE_XEN_DOMCTL_READ(max_mem, max_memkb);
      break;

   case VKI_XEN_DOMCTL_set_address_size:
      __PRE_XEN_DOMCTL_READ(set_address_size, address_size, size);
      break;

   case VKI_XEN_DOMCTL_settscinfo:
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.tsc_mode);
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.gtsc_khz);
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.incarnation);
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.elapsed_nsec);
      break;

   case VKI_XEN_DOMCTL_hypercall_init:
      PRE_XEN_DOMCTL_READ(hypercall_init, gmfn);
      break;

   case VKI_XEN_DOMCTL_settimeoffset:
       PRE_XEN_DOMCTL_READ(settimeoffset, time_offset_seconds);
       break;

   case VKI_XEN_DOMCTL_getvcpuinfo:
      PRE_XEN_DOMCTL_READ(getvcpuinfo, vcpu);
      break;

   case VKI_XEN_DOMCTL_scheduler_op:
      PRE_XEN_DOMCTL_READ(scheduler_op, sched_id);
      PRE_XEN_DOMCTL_READ(scheduler_op, cmd);
      if ( domctl->u.scheduler_op.cmd == VKI_XEN_DOMCTL_SCHEDOP_putinfo ) {
         switch(domctl->u.scheduler_op.sched_id) {
         case VKI_XEN_SCHEDULER_SEDF:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.period);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.slice);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.latency);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.extratime);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.weight);
            break;
         case VKI_XEN_SCHEDULER_CREDIT:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.credit.weight);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.credit.cap);
            break;
         case VKI_XEN_SCHEDULER_CREDIT2:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.credit2.weight);
            break;
         case VKI_XEN_SCHEDULER_ARINC653:
            break;
         }
      }
      break;

   case VKI_XEN_DOMCTL_getvcpuaffinity:
      __PRE_XEN_DOMCTL_READ(getvcpuaffinity, vcpuaffinity, vcpu);
      break;

   case VKI_XEN_DOMCTL_setvcpuaffinity:
      __PRE_XEN_DOMCTL_READ(setvcpuaffinity, vcpuaffinity, vcpu);
      PRE_MEM_READ("XEN_DOMCTL_setvcpuaffinity u.vcpuaffinity.cpumap.bitmap",
                   (Addr)domctl->u.vcpuaffinity.cpumap.bitmap.p,
                   domctl->u.vcpuaffinity.cpumap.nr_bits / 8);
      break;

   case VKI_XEN_DOMCTL_getnodeaffinity:
      __PRE_XEN_DOMCTL_READ(nodeaffinity, nodeaffinity, nodemap.nr_bits);
      break;
   case VKI_XEN_DOMCTL_setnodeaffinity:
      __PRE_XEN_DOMCTL_READ(nodeaffinity, nodeaffinity, nodemap.nr_bits);
      PRE_MEM_READ("XEN_DOMCTL_setnodeaffinity u.nodeaffinity.cpumap.bitmap",
                   (Addr)domctl->u.nodeaffinity.nodemap.bitmap.p,
                   domctl->u.nodeaffinity.nodemap.nr_bits / 8);
      break;

   case VKI_XEN_DOMCTL_getvcpucontext:
      __PRE_XEN_DOMCTL_READ(getvcpucontext, vcpucontext, vcpu);
      break;

   case VKI_XEN_DOMCTL_setvcpucontext:
      __PRE_XEN_DOMCTL_READ(setvcpucontext, vcpucontext, vcpu);
      __PRE_XEN_DOMCTL_READ(setvcpucontext, vcpucontext, ctxt.p);
      break;

   case VKI_XEN_DOMCTL_set_cpuid:
      PRE_MEM_READ("XEN_DOMCTL_set_cpuid u.cpuid",
                   (Addr)&domctl->u.cpuid, sizeof(domctl->u.cpuid));
      break;

   case VKI_XEN_DOMCTL_getpageframeinfo3:
       PRE_XEN_DOMCTL_READ(getpageframeinfo3, num);
       PRE_XEN_DOMCTL_READ(getpageframeinfo3, array.p);
       PRE_MEM_READ("XEN_DOMCTL_getpageframeinfo3 *u.getpageframeinfo3.array.p",
                    (Addr)domctl->u.getpageframeinfo3.array.p,
                    domctl->u.getpageframeinfo3.num * sizeof(vki_xen_pfn_t));
       break;

   case VKI_XEN_DOMCTL_getvcpuextstate:
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, vcpu);
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, xfeature_mask);
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, size);
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, buffer);
      break;

   case VKI_XEN_DOMCTL_shadow_op:
       PRE_XEN_DOMCTL_READ(shadow_op, op);

       switch(domctl->u.shadow_op.op)
       {
       case VKI_XEN_DOMCTL_SHADOW_OP_OFF:
           /* No further inputs */
           break;

       case VKI_XEN_DOMCTL_SHADOW_OP_ENABLE:
           PRE_XEN_DOMCTL_READ(shadow_op, mode);
           switch(domctl->u.shadow_op.mode)
           {
           case XEN_DOMCTL_SHADOW_ENABLE_LOG_DIRTY:
               goto domctl_shadow_op_enable_logdirty;


           default:
               bad_subop(tid, layout, arrghs, status, flags,
                         "__HYPERVISOR_domctl shadowop mode",
                         domctl->u.shadow_op.mode);
               break;
           }

       case VKI_XEN_DOMCTL_SHADOW_OP_ENABLE_LOGDIRTY:
       domctl_shadow_op_enable_logdirty:
           /* No further inputs */
           break;

       case VKI_XEN_DOMCTL_SHADOW_OP_CLEAN:
       case VKI_XEN_DOMCTL_SHADOW_OP_PEEK:
           PRE_XEN_DOMCTL_READ(shadow_op, dirty_bitmap);
           PRE_XEN_DOMCTL_READ(shadow_op, pages);
           break;

       default:
           bad_subop(tid, layout, arrghs, status, flags,
                     "__HYPERVISOR_domctl shadow(10)",
                     domctl->u.shadow_op.op);
           break;
       }
       break;

   case VKI_XEN_DOMCTL_set_max_evtchn:
      PRE_XEN_DOMCTL_READ(set_max_evtchn, max_port);
      break;

   case VKI_XEN_DOMCTL_cacheflush:
      PRE_XEN_DOMCTL_READ(cacheflush, start_pfn);
      PRE_XEN_DOMCTL_READ(cacheflush, nr_pfns);
      break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_domctl", domctl->cmd);
      break;
   }
#undef PRE_XEN_DOMCTL_READ
#undef __PRE_XEN_DOMCTL_READ
}

PRE(hvm_op)
{
   unsigned long op = ARG1;
   void *arg = (void *)(unsigned long)ARG2;

   PRINT("__HYPERVISOR_hvm_op ( %ld, %p )", op, arg);

#define __PRE_XEN_HVMOP_READ(_hvm_op, _type, _field)    \
   PRE_MEM_READ("XEN_HVMOP_" # _hvm_op " " #_field,     \
                (Addr)&((_type*)arg)->_field,           \
                sizeof(((_type*)arg)->_field))
#define PRE_XEN_HVMOP_READ(_hvm_op, _field)                             \
   __PRE_XEN_HVMOP_READ(_hvm_op, vki_xen_hvm_ ## _hvm_op ## _t, _field)

   switch (op) {
   case VKI_XEN_HVMOP_set_param:
      __PRE_XEN_HVMOP_READ(set_param, struct vki_xen_hvm_param, domid);
      __PRE_XEN_HVMOP_READ(set_param, struct vki_xen_hvm_param, index);
      __PRE_XEN_HVMOP_READ(set_param, struct vki_xen_hvm_param, value);
      break;

   case VKI_XEN_HVMOP_get_param:
      __PRE_XEN_HVMOP_READ(get_param, struct vki_xen_hvm_param, domid);
      __PRE_XEN_HVMOP_READ(get_param, struct vki_xen_hvm_param, index);
      break;

   case VKI_XEN_HVMOP_set_isa_irq_level:
       PRE_XEN_HVMOP_READ(set_isa_irq_level, domid);
       PRE_XEN_HVMOP_READ(set_isa_irq_level, isa_irq);
       PRE_XEN_HVMOP_READ(set_isa_irq_level, level);
       break;

   case VKI_XEN_HVMOP_set_pci_link_route:
       PRE_XEN_HVMOP_READ(set_pci_link_route, domid);
       PRE_XEN_HVMOP_READ(set_pci_link_route, link);
       PRE_XEN_HVMOP_READ(set_pci_link_route, isa_irq);
       break;

   case VKI_XEN_HVMOP_set_mem_type:
       PRE_XEN_HVMOP_READ(set_mem_type, domid);
       PRE_XEN_HVMOP_READ(set_mem_type, hvmmem_type);
       PRE_XEN_HVMOP_READ(set_mem_type, nr);
       PRE_XEN_HVMOP_READ(set_mem_type, first_pfn);
       break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_hvm_op", op);
      break;
   }
#undef __PRE_XEN_HVMOP_READ
#undef PRE_XEN_HVMOP_READ
}

PRE(tmem_op)
{
    struct vki_xen_tmem_op *tmem = (struct vki_xen_tmem_op *)ARG1;

    PRINT("__HYPERVISOR_tmem_op ( %d )", tmem->cmd);

    /* Common part for xen_tmem_op:
     *    vki_uint32_t cmd;
     */
    PRE_MEM_READ("__HYPERVISOR_tmem_op cmd", ARG1, sizeof(vki_uint32_t));


#define __PRE_XEN_TMEMOP_READ(_tmem, _union, _field)                    \
    PRE_MEM_READ("XEN_tmem_op_" #_tmem " u." #_union "." #_field,       \
                 (Addr)&tmem->u._union._field,                          \
                 sizeof(tmem->u._union._field))
#define PRE_XEN_TMEMOP_READ(_tmem, _field)                              \
    __PRE_XEN_TMEMOP_READ(_tmem, _tmem, _field)

    switch(tmem->cmd) {

    case VKI_XEN_TMEM_control:

        /* Common part for control hypercall:
         *    vki_int32_t pool_id;
         *    vki_uint32_t subop;
         */
        PRE_MEM_READ("__HYPERVISOR_tmem_op pool_id",
                     (Addr)&tmem->pool_id, sizeof(&tmem->pool_id));
        PRE_XEN_TMEMOP_READ(ctrl, subop);

        switch (tmem->u.ctrl.subop) {

        case VKI_XEN_TMEMC_save_begin:
            PRE_XEN_TMEMOP_READ(ctrl, cli_id);
            PRE_XEN_TMEMOP_READ(ctrl, arg1);
            PRE_XEN_TMEMOP_READ(ctrl, buf);
            break;

        default:
            bad_subop(tid, layout, arrghs, status, flags,
                      "__HYPERVISOR_tmem_op_control", tmem->u.ctrl.subop);
        }

        break;

    default:
        bad_subop(tid, layout, arrghs, status, flags,
                  "__HYPERVISOR_tmem_op", ARG1);
    }

#undef PRE_XEN_TMEMOP_READ
#undef __PRE_XEN_TMEMOP_READ
}

POST(memory_op)
{
   switch (ARG1) {
   case VKI_XENMEM_maximum_ram_page:
   case VKI_XENMEM_set_memory_map:
   case VKI_XENMEM_decrease_reservation:
   case VKI_XENMEM_claim_pages:
   case VKI_XENMEM_maximum_gpfn:
   case VKI_XENMEM_remove_from_physmap:
      /* No outputs */
      break;
   case VKI_XENMEM_increase_reservation:
   case VKI_XENMEM_populate_physmap: {
      struct xen_memory_reservation *memory_reservation =
         (struct xen_memory_reservation *)ARG2;

      POST_MEM_WRITE((Addr)memory_reservation->extent_start.p,
                     sizeof(vki_xen_pfn_t) * memory_reservation->nr_extents);
      break;
   }

   case VKI_XENMEM_machphys_mfn_list: {
       struct vki_xen_machphys_mfn_list *arg =
           (struct vki_xen_machphys_mfn_list *)ARG2;
       POST_MEM_WRITE((Addr)&arg->nr_extents, sizeof(arg->nr_extents));
       POST_MEM_WRITE((Addr)arg->extent_start.p,
                      sizeof(vki_xen_pfn_t) * arg->nr_extents);
       break;
   }

   case VKI_XENMEM_add_to_physmap: {
       struct vki_xen_add_to_physmap *arg =
           (struct vki_xen_add_to_physmap *)ARG2;
       if (arg->space == VKI_XENMAPSPACE_gmfn_range)
           POST_MEM_WRITE(ARG2, sizeof(*arg));
   }

   case VKI_XENMEM_get_sharing_freed_pages:
   case VKI_XENMEM_get_sharing_shared_pages:
       /* No outputs */
       break;
   }
}

POST(mmuext_op)
{
   unsigned int *pdone = (unsigned int *)ARG3;
   /* simplistic */
   POST_MEM_WRITE((Addr)pdone, sizeof(*pdone));
}

static void post_evtchn_op(ThreadId tid, __vki_u32 cmd, void *arg, int compat)
{
   switch (cmd) {
   case VKI_XEN_EVTCHNOP_alloc_unbound: {
      struct vki_xen_evtchn_alloc_unbound *alloc_unbound = arg;
      POST_MEM_WRITE((Addr)&alloc_unbound->port, sizeof(alloc_unbound->port));
      break;
   }
   }
}

POST(evtchn_op)
{
   post_evtchn_op(tid, ARG1, (void *)ARG2, 0);
}

POST(evtchn_op_compat)
{
   struct vki_xen_evtchn_op *evtchn = (struct vki_xen_evtchn_op *)ARG1;
   post_evtchn_op(tid, evtchn->cmd, &evtchn->u, 1);
}

POST(xen_version)
{
   switch (ARG1) {
   case VKI_XENVER_version:
      /* No outputs */
      break;
   case VKI_XENVER_extraversion:
      POST_MEM_WRITE((Addr)ARG2, sizeof(vki_xen_extraversion_t));
      break;
   case VKI_XENVER_compile_info:
      POST_MEM_WRITE((Addr)ARG2, sizeof(struct vki_xen_compile_info));
      break;
   case VKI_XENVER_capabilities:
      POST_MEM_WRITE((Addr)ARG2, sizeof(vki_xen_capabilities_info_t));
      break;
   case VKI_XENVER_changeset:
      POST_MEM_WRITE((Addr)ARG2, sizeof(vki_xen_changeset_info_t));
      break;
   case VKI_XENVER_platform_parameters:
      POST_MEM_WRITE((Addr)ARG2, sizeof(struct vki_xen_platform_parameters));
      break;
   case VKI_XENVER_get_features:
      POST_MEM_WRITE((Addr)ARG2, sizeof(struct vki_xen_feature_info));
      break;
   case VKI_XENVER_pagesize:
      /* No outputs */
      break;
   case VKI_XENVER_guest_handle:
      POST_MEM_WRITE((Addr)ARG2, sizeof(vki_xen_domain_handle_t));
      break;
   case VKI_XENVER_commandline:
      POST_MEM_WRITE((Addr)ARG2, sizeof(vki_xen_commandline_t));
      break;
   }
}

POST(grant_table_op)
{
   switch (ARG1) {
   case VKI_XEN_GNTTABOP_setup_table: {
      struct vki_xen_gnttab_setup_table *gst =
	      (struct vki_xen_gnttab_setup_table*)ARG2;
      PRE_MEM_WRITE("VKI_XEN_GNTTABOP_setup_table",
                    (Addr)&gst->status, sizeof(gst->status));
      PRE_MEM_WRITE("VKI_XEN_GNTTABOP_setup_table",
                    (Addr)gst->frame_list.p,
                    sizeof(*gst->frame_list.p) & gst->nr_frames);
      break;
   }
   }
}

POST(sysctl)
{
   struct vki_xen_sysctl *sysctl = (struct vki_xen_sysctl *)ARG1;

   switch (sysctl->interface_version)
   {
   case 0x00000008:
   case 0x00000009:
   case 0x0000000a:
	   break;
   default:
      return;
   }

#define __POST_XEN_SYSCTL_WRITE(_sysctl, _union, _field)        \
      POST_MEM_WRITE((Addr)&sysctl->u._union._field,            \
                     sizeof(sysctl->u._union._field))
#define POST_XEN_SYSCTL_WRITE(_sysctl, _field) \
      __POST_XEN_SYSCTL_WRITE(_sysctl, _sysctl, _field)

   switch (sysctl->cmd) {
   case VKI_XEN_SYSCTL_readconsole:
       POST_MEM_WRITE((Addr)sysctl->u.readconsole.buffer.p,
                      sysctl->u.readconsole.count * sizeof(char));
       break;

   case VKI_XEN_SYSCTL_getdomaininfolist:
      switch (sysctl->interface_version)
      {
      case 0x00000008:
	 POST_XEN_SYSCTL_WRITE(getdomaininfolist_00000008, num_domains);
	 POST_MEM_WRITE((Addr)sysctl->u.getdomaininfolist_00000008.buffer.p,
			sizeof(*sysctl->u.getdomaininfolist_00000008.buffer.p)
			* sysctl->u.getdomaininfolist_00000008.num_domains);
	 break;
      case 0x00000009:
	 POST_XEN_SYSCTL_WRITE(getdomaininfolist_00000009, num_domains);
	 POST_MEM_WRITE((Addr)sysctl->u.getdomaininfolist_00000009.buffer.p,
			sizeof(*sysctl->u.getdomaininfolist_00000009.buffer.p)
			* sysctl->u.getdomaininfolist_00000009.num_domains);
	 break;
      case 0x0000000a:
	 POST_XEN_SYSCTL_WRITE(getdomaininfolist_0000000a, num_domains);
	 POST_MEM_WRITE((Addr)sysctl->u.getdomaininfolist_0000000a.buffer.p,
			sizeof(*sysctl->u.getdomaininfolist_0000000a.buffer.p)
			* sysctl->u.getdomaininfolist_0000000a.num_domains);
	 break;
      }
      break;

   case VKI_XEN_SYSCTL_sched_id:
       POST_XEN_SYSCTL_WRITE(sched_id, sched_id);
       break;

   case VKI_XEN_SYSCTL_cpupool_op:
      if (sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_CREATE ||
          sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_INFO)
         POST_XEN_SYSCTL_WRITE(cpupool_op, cpupool_id);
      if (sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_INFO) {
         POST_XEN_SYSCTL_WRITE(cpupool_op, sched_id);
         POST_XEN_SYSCTL_WRITE(cpupool_op, n_dom);
      }
      if (sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_INFO ||
          sysctl->u.cpupool_op.op == VKI_XEN_SYSCTL_CPUPOOL_OP_FREEINFO)
         POST_XEN_SYSCTL_WRITE(cpupool_op, cpumap);
      break;

   case VKI_XEN_SYSCTL_physinfo:
      switch (sysctl->interface_version)
      {
      case 0x00000008:
      case 0x00000009: /* Unchanged from version 8 */
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, threads_per_core);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, cores_per_socket);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, nr_cpus);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, max_cpu_id);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, nr_nodes);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, max_node_id);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, cpu_khz);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, total_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, free_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, scrub_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, hw_cap[8]);
         POST_XEN_SYSCTL_WRITE(physinfo_00000008, capabilities);
         break;
      case 0x0000000a:
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, threads_per_core);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, cores_per_socket);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, nr_cpus);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, max_cpu_id);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, nr_nodes);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, max_node_id);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, cpu_khz);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, total_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, free_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, scrub_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, outstanding_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, hw_cap[8]);
         POST_XEN_SYSCTL_WRITE(physinfo_0000000a, capabilities);
         break;
      }
      break;

   case VKI_XEN_SYSCTL_topologyinfo:
      POST_XEN_SYSCTL_WRITE(topologyinfo, max_cpu_index);
      if (sysctl->u.topologyinfo.cpu_to_core.p)
         POST_MEM_WRITE((Addr)sysctl->u.topologyinfo.cpu_to_core.p,
                     sizeof(uint32_t) * sysctl->u.topologyinfo.max_cpu_index);
      if (sysctl->u.topologyinfo.cpu_to_socket.p)
         POST_MEM_WRITE((Addr)sysctl->u.topologyinfo.cpu_to_socket.p,
                     sizeof(uint32_t) * sysctl->u.topologyinfo.max_cpu_index);
      if (sysctl->u.topologyinfo.cpu_to_node.p)
         POST_MEM_WRITE((Addr)sysctl->u.topologyinfo.cpu_to_node.p,
                     sizeof(uint32_t) * sysctl->u.topologyinfo.max_cpu_index);
      break;

   case VKI_XEN_SYSCTL_numainfo:
      POST_XEN_SYSCTL_WRITE(numainfo, max_node_index);
      POST_MEM_WRITE((Addr)sysctl->u.numainfo.node_to_memsize.p,
                     sizeof(uint64_t) * sysctl->u.numainfo.max_node_index);
      POST_MEM_WRITE((Addr)sysctl->u.numainfo.node_to_memfree.p,
                     sizeof(uint64_t) * sysctl->u.numainfo.max_node_index);
      POST_MEM_WRITE((Addr)sysctl->u.numainfo.node_to_node_distance.p,
                     sizeof(uint32_t) * sysctl->u.numainfo.max_node_index);
      break;

   /* No outputs */
   case VKI_XEN_SYSCTL_debug_keys:
       break;
   }
#undef POST_XEN_SYSCTL_WRITE
#undef __POST_XEN_SYSCTL_WRITE
}

POST(domctl){
   struct vki_xen_domctl *domctl = (struct vki_xen_domctl *)ARG1;

   switch (domctl->interface_version) {
   case 0x00000007:
   case 0x00000008:
   case 0x00000009:
	   break;
   default:
	   return;
   }

#define __POST_XEN_DOMCTL_WRITE(_domctl, _union, _field)        \
   POST_MEM_WRITE((Addr)&domctl->u._union._field,               \
                  sizeof(domctl->u._union._field));
#define POST_XEN_DOMCTL_WRITE(_domctl, _field)          \
   __POST_XEN_DOMCTL_WRITE(_domctl, _domctl, _field)

   switch (domctl->cmd) {
   case VKI_XEN_DOMCTL_createdomain:
   case VKI_XEN_DOMCTL_destroydomain:
   case VKI_XEN_DOMCTL_pausedomain:
   case VKI_XEN_DOMCTL_max_mem:
   case VKI_XEN_DOMCTL_set_address_size:
   case VKI_XEN_DOMCTL_settscinfo:
   case VKI_XEN_DOMCTL_hypercall_init:
   case VKI_XEN_DOMCTL_setvcpuaffinity:
   case VKI_XEN_DOMCTL_setvcpucontext:
   case VKI_XEN_DOMCTL_setnodeaffinity:
   case VKI_XEN_DOMCTL_set_cpuid:
   case VKI_XEN_DOMCTL_unpausedomain:
   case VKI_XEN_DOMCTL_sethvmcontext:
   case VKI_XEN_DOMCTL_set_max_evtchn:
   case VKI_XEN_DOMCTL_cacheflush:
   case VKI_XEN_DOMCTL_resumedomain:
      /* No output fields */
      break;

   case VKI_XEN_DOMCTL_max_vcpus:
      POST_XEN_DOMCTL_WRITE(max_vcpus, max);
      break;

   case VKI_XEN_DOMCTL_get_address_size:
      __POST_XEN_DOMCTL_WRITE(get_address_size, address_size, size);
      break;

   case VKI_XEN_DOMCTL_gettscinfo:
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.tsc_mode);
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.gtsc_khz);
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.incarnation);
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.elapsed_nsec);
      break;

   case VKI_XEN_DOMCTL_getvcpuinfo:
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, online);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, blocked);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, running);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, cpu_time);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, cpu);
      break;

   case VKI_XEN_DOMCTL_gethvmcontext:
       /* Xen unconditionally writes size... */
       __POST_XEN_DOMCTL_WRITE(gethvmcontext, hvmcontext, size);
       /* ...but only writes to the buffer if it was non NULL */
       if ( domctl->u.hvmcontext.buffer.p )
           POST_MEM_WRITE((Addr)domctl->u.hvmcontext.buffer.p,
                          sizeof(*domctl->u.hvmcontext.buffer.p)
                          * domctl->u.hvmcontext.size);
       break;

   case VKI_XEN_DOMCTL_scheduler_op:
      if ( domctl->u.scheduler_op.cmd == VKI_XEN_DOMCTL_SCHEDOP_getinfo ) {
         switch(domctl->u.scheduler_op.sched_id) {
         case VKI_XEN_SCHEDULER_SEDF:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.period);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.slice);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.latency);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.extratime);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.weight);
            break;
         case VKI_XEN_SCHEDULER_CREDIT:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.credit.weight);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.credit.cap);
            break;
         case VKI_XEN_SCHEDULER_CREDIT2:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.credit2.weight);
            break;
         case VKI_XEN_SCHEDULER_ARINC653:
            break;
         }
      }
      break;

   case VKI_XEN_DOMCTL_getvcpuaffinity:
      POST_MEM_WRITE((Addr)domctl->u.vcpuaffinity.cpumap.bitmap.p,
                     domctl->u.vcpuaffinity.cpumap.nr_bits / 8);
      break;

   case VKI_XEN_DOMCTL_getnodeaffinity:
      POST_MEM_WRITE((Addr)domctl->u.nodeaffinity.nodemap.bitmap.p,
                     domctl->u.nodeaffinity.nodemap.nr_bits / 8);
      break;

   case VKI_XEN_DOMCTL_getdomaininfo:
      switch (domctl->interface_version) {
      case 0x00000007:
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, domain);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, flags);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, tot_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, max_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, shr_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, shared_info_frame);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, cpu_time);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, nr_online_vcpus);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, max_vcpu_id);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, ssidref);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, handle);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000007, cpupool);
      break;
      case 0x00000008:
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, domain);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, flags);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, tot_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, max_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, shr_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, paged_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, shared_info_frame);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, cpu_time);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, nr_online_vcpus);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, max_vcpu_id);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, ssidref);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, handle);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000008, cpupool);
      break;
      case 0x00000009:
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, domain);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, flags);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, tot_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, max_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, outstanding_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, shr_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, paged_pages);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, shared_info_frame);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, cpu_time);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, nr_online_vcpus);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, max_vcpu_id);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, ssidref);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, handle);
	 POST_XEN_DOMCTL_WRITE(getdomaininfo_00000009, cpupool);
      break;
      }
      break;
   case VKI_XEN_DOMCTL_getvcpucontext:
      __POST_XEN_DOMCTL_WRITE(getvcpucontext, vcpucontext, ctxt.p);
      break;

   case VKI_XEN_DOMCTL_getpageframeinfo3:
       POST_MEM_WRITE((Addr)domctl->u.getpageframeinfo3.array.p,
                      domctl->u.getpageframeinfo3.num * sizeof(vki_xen_pfn_t));
       break;


   case VKI_XEN_DOMCTL_getvcpuextstate:
      __POST_XEN_DOMCTL_WRITE(getvcpuextstate, vcpuextstate, xfeature_mask);
      __POST_XEN_DOMCTL_WRITE(getvcpuextstate, vcpuextstate, size);
      POST_MEM_WRITE((Addr)domctl->u.vcpuextstate.buffer.p,
                     domctl->u.vcpuextstate.size);
      break;

   case VKI_XEN_DOMCTL_shadow_op:
       switch(domctl->u.shadow_op.op)
       {
       case VKI_XEN_DOMCTL_SHADOW_OP_OFF:
           /* No outputs */
           break;

       case VKI_XEN_DOMCTL_SHADOW_OP_CLEAN:
       case VKI_XEN_DOMCTL_SHADOW_OP_PEEK:
           POST_XEN_DOMCTL_WRITE(shadow_op, pages);
           POST_XEN_DOMCTL_WRITE(shadow_op, stats.fault_count);
           POST_XEN_DOMCTL_WRITE(shadow_op, stats.dirty_count);
           if(domctl->u.shadow_op.dirty_bitmap.p)
               POST_MEM_WRITE((Addr)domctl->u.shadow_op.dirty_bitmap.p,
                              domctl->u.shadow_op.pages * sizeof(vki_uint8_t));
           break;

       default:
           break;
       }
       break;
   }
#undef POST_XEN_DOMCTL_WRITE
#undef __POST_XEN_DOMCTL_WRITE
}

POST(hvm_op)
{
   unsigned long op = ARG1;
   void *arg = (void *)(unsigned long)ARG2;

#define __POST_XEN_HVMOP_WRITE(_hvm_op, _type, _field)  \
      POST_MEM_WRITE((Addr)&((_type*)arg)->_field,      \
                     sizeof(((_type*)arg)->_field))
#define POST_XEN_HVMOP_WRITE(_hvm_op, _field) \
      __PRE_XEN_HVMOP_READ(_hvm_op, vki_xen_hvm_ ## _hvm_op ## _t, _field)

   switch (op) {
   case VKI_XEN_HVMOP_set_param:
   case VKI_XEN_HVMOP_set_isa_irq_level:
   case VKI_XEN_HVMOP_set_pci_link_route:
   case VKI_XEN_HVMOP_set_mem_type:
      /* No output paramters */
      break;

   case VKI_XEN_HVMOP_get_param:
      __POST_XEN_HVMOP_WRITE(get_param, struct vki_xen_hvm_param, value);
      break;
   }
#undef __POST_XEN_HVMOP_WRITE
#undef POST_XEN_HVMOP_WRITE
}

POST(tmem_op)
{
    struct vki_xen_tmem_op *tmem = (struct vki_xen_tmem_op *)ARG1;

    switch(tmem->cmd) {

    case VKI_XEN_TMEM_control:

        switch(tmem->u.ctrl.subop) {
            /* No outputs */
            case VKI_XEN_TMEMC_save_begin:
                break;
        }

        break;
    }
}

typedef
   struct {
      SyscallTableEntry entry;
      int nr_args;
   }
   XenHypercallTableEntry;

#define HYPX_(const, name, nr_args) \
   [const] = { { vgSysWrap_xen_##name##_before, NULL }, nr_args }
#define HYPXY(const, name, nr_args)                     \
   [const] = { { vgSysWrap_xen_##name##_before,         \
                 vgSysWrap_xen_##name##_after },        \
               nr_args }

static XenHypercallTableEntry hypercall_table[] = {
   //    __VKI_XEN_set_trap_table                                  // 0
   //    __VKI_XEN_mmu_update                                      // 1
   //    __VKI_XEN_set_gdt                                         // 2
   //    __VKI_XEN_stack_switch                                    // 3
   //    __VKI_XEN_set_callbacks                                   // 4

   //    __VKI_XEN_fpu_taskswitch                                  // 5
   //    __VKI_XEN_sched_op_compat                                 // 6
   //    __VKI_XEN_platform_op                                     // 7
   //    __VKI_XEN_set_debugreg                                    // 8
   //    __VKI_XEN_get_debugreg                                    // 9

   //    __VKI_XEN_update_descriptor                               // 10
   //                                                                 // 11
   HYPXY(__VKI_XEN_memory_op,               memory_op,         2), // 12
   //    __VKI_XEN_multicall                                       // 13
   //    __VKI_XEN_update_va_mapping                               // 14

   //    __VKI_XEN_set_timer_op                                    // 15
   HYPXY(__VKI_XEN_event_channel_op_compat, evtchn_op_compat,  1), // 16
   HYPXY(__VKI_XEN_xen_version,             xen_version,       2), // 17
   //    __VKI_XEN_console_io                                      // 18
   //    __VKI_XEN_physdev_op_compat                               // 19

   HYPXY(__VKI_XEN_grant_table_op,          grant_table_op,    3), // 20
   //    __VKI_XEN_vm_assist                                       // 21
   //    __VKI_XEN_update_va_mapping_otherdomain                   // 22
   //    __VKI_XEN_iret,                    iret                   // 23
   //    __VKI_XEN_vcpu_op,                 vcpu_op                // 24

   //    __VKI_XEN_set_segment_base                                // 25
   HYPXY(__VKI_XEN_mmuext_op,               mmuext_op,         2), // 26
   //    __VKI_XEN_xsm_op                                          // 27
   //    __VKI_XEN_nmi_op                                          // 28
   //    __VKI_XEN_sched_op                                        // 29

   //    __VKI_XEN_callback_op                                     // 30
   //    __VKI_XEN_xenoprof_op                                     // 31
   HYPXY(__VKI_XEN_event_channel_op,        evtchn_op,         2), // 32
   //    __VKI_XEN_physdev_op                                      // 33
   HYPXY(__VKI_XEN_hvm_op,                  hvm_op,            2), // 34

   HYPXY(__VKI_XEN_sysctl,                  sysctl,            1), // 35
   HYPXY(__VKI_XEN_domctl,                  domctl,            1), // 36
   //    __VKI_XEN_kexec_op                                        // 37
   HYPXY(__VKI_XEN_tmem_op,                 tmem_op,           1), // 38
};

static void bad_before ( ThreadId              tid,
                         SyscallArgLayout*     layout,
                         /*MOD*/SyscallArgs*   args,
                         /*OUT*/SyscallStatus* status,
                         /*OUT*/UWord*         flags )
{
   VG_(dmsg)("WARNING: unhandled hypercall: %s\n",
      VG_SYSNUM_STRING_EXTRA(args->sysno));
   if (VG_(clo_verbosity) > 1) {
      VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
   }
   VG_(dmsg)("You may be able to write your own handler.\n");
   VG_(dmsg)("Read the file README_MISSING_SYSCALL_OR_IOCTL.\n");
   VG_(dmsg)("Nevertheless we consider this a bug.  Please report\n");
   VG_(dmsg)("it at http://valgrind.org/support/bug_reports.html &\n");
   VG_(dmsg)("http://wiki.xen.org/wiki/Reporting_Bugs_against_Xen.\n");

   SET_STATUS_Failure(VKI_ENOSYS);
}

static XenHypercallTableEntry bad_hyper =
{ { bad_before, NULL }, 0 };

static XenHypercallTableEntry* ML_(get_xen_hypercall_entry) ( UInt sysno )
{
   XenHypercallTableEntry *ret = &bad_hyper;

   const UInt hypercall_table_size
      = sizeof(hypercall_table) / sizeof(hypercall_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < hypercall_table_size) {
      XenHypercallTableEntry* ent = &hypercall_table[sysno];
      if (ent->entry.before != NULL)
         ret = ent;
   }

   /* Can't find a wrapper */
   return ret;
}

DEFN_PRE_TEMPLATE(xen, hypercall)
{
   XenHypercallTableEntry *ent = ML_(get_xen_hypercall_entry)(SYSNO);

   /* Return number of arguments consumed */
   ARG8 = ent->nr_args;

   vg_assert(ent);
   vg_assert(ent->entry.before);
   (ent->entry.before)( tid, layout, arrghs, status, flags );

}

DEFN_POST_TEMPLATE(xen, hypercall)
{
   XenHypercallTableEntry *ent = ML_(get_xen_hypercall_entry)(SYSNO);

   /* Return number of arguments consumed */
   ARG8 = ent->nr_args;

   vg_assert(ent);
   if (ent->entry.after)
      (ent->entry.after)( tid, arrghs, status );
}

#endif // defined(ENABLE_XEN)
