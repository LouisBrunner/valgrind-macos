
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"

#if defined(ENABLE_XEN)

#include "pub_core_vkiscnums.h"
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

static void bad_intf_version ( ThreadId              tid,
                               SyscallArgLayout*     layout,
                               /*MOD*/SyscallArgs*   args,
                               /*OUT*/SyscallStatus* status,
                               /*OUT*/UWord*         flags,
                               const HChar*          hypercall,
                               UWord                 version)
{
   VG_(dmsg)("WARNING: %s version %#lx not supported\n",
             hypercall, version);
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

static void bad_subop ( ThreadId              tid,
                        SyscallArgLayout*     layout,
                        /*MOD*/SyscallArgs*   args,
                        /*OUT*/SyscallStatus* status,
                        /*OUT*/UWord*         flags,
                        const HChar*          hypercall,
                        UWord                 subop)
{
   VG_(dmsg)("WARNING: unhandled %s subop: %lu\n",
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
   PRINT("__HYPERVISOR_memory_op ( %lu, %#lx )", ARG1, ARG2);

   switch (ARG1) {

   case VKI_XENMEM_maximum_ram_page:
       /* No inputs */
       break;

   case VKI_XENMEM_maximum_gpfn:
       PRE_MEM_READ("XENMEM_maximum_gpfn domid",
                    (Addr)ARG2, sizeof(vki_xen_domid_t));
       break;

   case VKI_XENMEM_machphys_mfn_list:
   case VKI_XENMEM_machphys_compat_mfn_list: {
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

   case VKI_XENMEM_memory_map:
   case VKI_XENMEM_machine_memory_map: {
      struct vki_xen_memory_map *arg =
	      (struct vki_xen_memory_map *)ARG2;
      PRE_MEM_READ("XENMEM_memory_map nr_entries",
                   (Addr)&arg->nr_entries, sizeof(arg->nr_entries));
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
   }

   case VKI_XENMEM_remove_from_physmap: {
       struct vki_xen_remove_from_physmap *arg =
           (struct vki_xen_remove_from_physmap *)ARG2;
       PRE_MEM_READ("XENMEM_remove_from_physmap domid",
                    (Addr)&arg->domid, sizeof(arg->domid));
       PRE_MEM_READ("XENMEM_remove_from_physmap gpfn",
                    (Addr)&arg->gpfn, sizeof(arg->gpfn));
       break;
   }

   case VKI_XENMEM_get_sharing_freed_pages:
   case VKI_XENMEM_get_sharing_shared_pages:
      break;

   case VKI_XENMEM_access_op: {
       struct vki_xen_mem_event_op *arg =
            (struct vki_xen_mem_event_op *)ARG2;
       PRE_MEM_READ("XENMEM_access_op domid",
                    (Addr)&arg->domain, sizeof(arg->domain));
       PRE_MEM_READ("XENMEM_access_op op",
                    (Addr)&arg->op, sizeof(arg->op));
       PRE_MEM_READ("XENMEM_access_op gfn",
                    (Addr)&arg->gfn, sizeof(arg->gfn));
       break;
   }
   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_memory_op", ARG1);
      break;
   }
}

PRE(mmuext_op)
{
   PRINT("__HYPERVISOR_mmuext_op ( %#lx, %ld, %#lx, %lu )",
         ARG1, SARG2, ARG3, ARG4);

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

PRE(xsm_op)
{
   /* XXX assuming flask, only actual XSM right now */
   struct vki_xen_flask_op *op = (struct vki_xen_flask_op *)ARG1;

   PRINT("__HYPERVISOR_xsm_op ( %u )", op->cmd);

   /*
    * Common part of xen_flask_op:
    *    vki_uint32_t cmd;
    *    vki_uint32_t interface_version;
    */
   PRE_MEM_READ("__HYPERVISOR_xsm_op", ARG1,
                sizeof(vki_uint32_t) + sizeof(vki_uint32_t));

   if (!op)
      return;

   switch (op->interface_version) {
   case 0x00000001:
      break;
   default:
      bad_intf_version(tid, layout, arrghs, status, flags,
                       "__HYPERVISOR_xsm_op", op->interface_version);
      return;
   }

#define PRE_XEN_XSM_OP_READ(_xsm_op, _union, _field)            \
   PRE_MEM_READ("FLASK_" #_xsm_op " u." #_union "." #_field,    \
                (Addr)&op->u._union._field,                     \
                sizeof(op->u._union._field))

   switch (op->cmd) {
   case VKI_FLASK_SID_TO_CONTEXT:
      PRE_XEN_XSM_OP_READ(SID_TO_CONTEXT, sid_context, sid);
      PRE_XEN_XSM_OP_READ(SID_TO_CONTEXT, sid_context, size);
      PRE_XEN_XSM_OP_READ(SID_TO_CONTEXT, sid_context, context.p);
      break;
   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_xsm_op", op->cmd);
      break;
   }
#undef __PRE_XEN_XSM_OP_READ
#undef PRE_XEN_XSM_OP_READ
}

PRE(sched_op)
{
   PRINT("__HYPERVISOR_sched_op ( %ld, %#lx )", SARG1, ARG2);
   void *arg = (void *)(unsigned long)ARG2;

#define __PRE_XEN_SCHEDOP_READ(_schedop, _type, _field) \
   PRE_MEM_READ("XEN_SCHEDOP_" # _schedop " " #_field,  \
                (Addr)&((_type*)arg)->_field,           \
                sizeof(((_type*)arg)->_field))
#define PRE_XEN_SCHEDOP_READ(_schedop, _field)                          \
   __PRE_XEN_SCHEDOP_READ(_schedop, vki_xen_ ## _schedop ## _t, _field)

   switch (ARG1) {
   case VKI_XEN_SCHEDOP_remote_shutdown:
      PRE_XEN_SCHEDOP_READ(remote_shutdown, domain_id);
      PRE_XEN_SCHEDOP_READ(remote_shutdown, reason);
      break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_sched_op", ARG1);
      break;
   }
#undef __PRE_XEN_SCHEDOP_READ
#undef PRE_XEN_SCHEDOP_READ
}

static void pre_evtchn_op(ThreadId tid,
                          SyscallArgLayout*     layout,
                          /*MOD*/SyscallArgs*   arrghs,
                          /*OUT*/SyscallStatus* status,
                          /*OUT*/UWord*         flags,
                          __vki_u32 cmd, void *arg, int compat)
{
   PRINT("__HYPERVISOR_event_channel_op%s ( %u, %p )",
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

PRE(physdev_op)
{
   int cmd = ARG1;

   PRINT("__HYPERVISOR_physdev_op ( %ld, %#lx )", SARG1, ARG2);

#define PRE_XEN_PHYSDEVOP_READ(_op, _field)		\
   PRE_MEM_READ("XEN_PHYSDEVOP_" #_op " ." #_field,	\
                (Addr)&arg->_field,			\
                sizeof(arg->_field))

   switch (cmd) {
   case VKI_XEN_PHYSDEVOP_map_pirq: {
      struct vki_xen_physdev_map_pirq *arg =
         (struct vki_xen_physdev_map_pirq *)ARG2;

      PRE_XEN_PHYSDEVOP_READ("map_pirq", domid);
      PRE_XEN_PHYSDEVOP_READ("map_pirq", type);

      PRE_XEN_PHYSDEVOP_READ("map_pirq", bus);
      PRE_XEN_PHYSDEVOP_READ("map_pirq", devfn);
      PRE_XEN_PHYSDEVOP_READ("map_pirq", entry_nr);
      PRE_XEN_PHYSDEVOP_READ("map_pirq", table_base);

      switch(arg->type) {
      case VKI_XEN_MAP_PIRQ_TYPE_MSI:
         PRE_XEN_PHYSDEVOP_READ("map_pirq", index);
         break;
      case VKI_XEN_MAP_PIRQ_TYPE_GSI:
         PRE_XEN_PHYSDEVOP_READ("map_pirq", index);
         PRE_XEN_PHYSDEVOP_READ("map_pirq", pirq);
         break;
      case VKI_XEN_MAP_PIRQ_TYPE_MSI_SEG:
         PRE_XEN_PHYSDEVOP_READ("map_pirq", index);
         break;
      case VKI_XEN_MAP_PIRQ_TYPE_MULTI_MSI:
         break;
      }
      break;
   }
   case VKI_XEN_PHYSDEVOP_unmap_pirq: {
      struct vki_xen_physdev_unmap_pirq *arg =
         (struct vki_xen_physdev_unmap_pirq *)ARG2;
      PRE_XEN_PHYSDEVOP_READ("unmap_pirq", domid);
      PRE_XEN_PHYSDEVOP_READ("unmap_pirq", pirq);
      break;
   }
   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_physdev_op", cmd);
   }
#undef PRE_XEN_PHYSDEVOP_READ
}

PRE(xen_version)
{
   PRINT("__HYPERVISOR_xen_version ( %ld, %#lx )", SARG1, ARG2);

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
   PRINT("__HYPERVISOR_grant_table_op ( %lu, %#lx, %lu )", ARG1, ARG2, ARG3);
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

   PRINT("__HYPERVISOR_sysctl ( %u )", sysctl->cmd);

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
   case 0x0000000b:
   case 0x0000000c:
   case 0x0000000d:
   case 0x0000000e:
   case 0x0000000f:
   case 0x00000010:
   case 0x00000011:
   case 0x00000012:
	   break;
   default:
      bad_intf_version(tid, layout, arrghs, status, flags,
                       "__HYPERVISOR_sysctl", sysctl->interface_version);
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
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_0000000a, first_domain);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_0000000a, max_domains);
	 PRE_XEN_SYSCTL_READ(getdomaininfolist_0000000a, buffer);
	 break;
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
     PRE_XEN_SYSCTL_READ(getdomaininfolist_00000010, first_domain);
     PRE_XEN_SYSCTL_READ(getdomaininfolist_00000010, max_domains);
     PRE_XEN_SYSCTL_READ(getdomaininfolist_00000010, buffer);
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

   PRINT("__HYPERVISOR_domctl ( %u ) on dom%d", domctl->cmd, domctl->domain);

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
   case 0x0000000a:
   case 0x0000000b:
   case 0x0000000c:
   case 0x0000000d:
   case 0x0000000f:
   case 0x00000010:
   case 0x00000011:
   case 0x00000012:
	   break;
   default:
      bad_intf_version(tid, layout, arrghs, status, flags,
                       "__HYPERVISOR_domctl", domctl->interface_version);
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

   case VKI_XEN_DOMCTL_gethvmcontext_partial:
       __PRE_XEN_DOMCTL_READ(gethvmcontext_partial, hvmcontext_partial_00000007, type);
       __PRE_XEN_DOMCTL_READ(gethvmcontext_partial, hvmcontext_partial_00000007, instance);
       __PRE_XEN_DOMCTL_READ(gethvmcontext_partial, hvmcontext_partial_00000007, buffer);

       switch (domctl->u.hvmcontext_partial_00000007.type) {
       case VKI_HVM_SAVE_CODE(CPU):
           if ( domctl->u.hvmcontext_partial_00000007.buffer.p )
                PRE_MEM_WRITE("XEN_DOMCTL_gethvmcontext_partial *buffer",
                   (Addr)domctl->u.hvmcontext_partial_00000007.buffer.p,
                   VKI_HVM_SAVE_LENGTH(CPU));
           break;
       case VKI_HVM_SAVE_CODE(MTRR):
           if ( domctl->u.hvmcontext_partial_00000007.buffer.p )
	        PRE_MEM_WRITE("XEN_DOMCTL_gethvmcontext_partial *buffer",
		   (Addr)domctl->u.hvmcontext_partial_00000007.buffer.p,
		   VKI_HVM_SAVE_LENGTH(MTRR));
           break;
       default:
           bad_subop(tid, layout, arrghs, status, flags,
                         "__HYPERVISOR_domctl_gethvmcontext_partial type",
                         domctl->u.hvmcontext_partial_00000007.type);
           break;
       }
       break;

   case VKI_XEN_DOMCTL_max_mem:
      PRE_XEN_DOMCTL_READ(max_mem, max_memkb);
      break;

   case VKI_XEN_DOMCTL_set_address_size:
      __PRE_XEN_DOMCTL_READ(set_address_size, address_size, size);
      break;

   case VKI_XEN_DOMCTL_test_assign_device:
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __PRE_XEN_DOMCTL_READ(test_assign_device, assign_device_00000007, machine_sbdf);
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(test_assign_device, assign_device_0000000b, dev);
         __PRE_XEN_DOMCTL_READ(test_assign_device, assign_device_0000000b, flag);
         switch (domctl->u.assign_device_0000000b.dev) {
         case VKI_XEN_DOMCTL_DEV_PCI:
            __PRE_XEN_DOMCTL_READ(test_assign_device, assign_device_0000000b, u.pci);
            break;
         case VKI_XEN_DOMCTL_DEV_DT:
            __PRE_XEN_DOMCTL_READ(test_assign_device, assign_device_0000000b, u.dt);
            PRE_MEM_READ("XEN_DOMTCL_test_assign_device.dt",
                          (Addr)domctl->u.assign_device_0000000b.u.dt.path.p,
                          domctl->u.assign_device_0000000b.u.dt.size);
            break;
         default:
            bad_subop(tid, layout, arrghs, status, flags,
                         "__HYPERVISOR_domctl_test_assign_device dev",
                         domctl->u.assign_device_0000000b.dev);
            break;
         }
         break;
      }
      break;
   case VKI_XEN_DOMCTL_assign_device:
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __PRE_XEN_DOMCTL_READ(assign_device, assign_device_00000007, machine_sbdf);
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(assign_device, assign_device_0000000b, dev);
         __PRE_XEN_DOMCTL_READ(assign_device, assign_device_0000000b, flag);
         switch (domctl->u.assign_device_0000000b.dev) {
         case VKI_XEN_DOMCTL_DEV_PCI:
            __PRE_XEN_DOMCTL_READ(assign_device, assign_device_0000000b, u.pci);
            break;
         case VKI_XEN_DOMCTL_DEV_DT:
            __PRE_XEN_DOMCTL_READ(assign_device, assign_device_0000000b, u.dt);
            PRE_MEM_READ("XEN_DOMTCL_assign_device.dt",
                          (Addr)domctl->u.assign_device_0000000b.u.dt.path.p,
                          domctl->u.assign_device_0000000b.u.dt.size);
            break;
         default:
            bad_subop(tid, layout, arrghs, status, flags,
                         "__HYPERVISOR_domctl_assign_device dev",
                         domctl->u.assign_device_0000000b.dev);
            break;
         }
         break;
      }
      break;
   case VKI_XEN_DOMCTL_deassign_device:
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __PRE_XEN_DOMCTL_READ(deassign_device, assign_device_00000007, machine_sbdf);
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(deassign_device, assign_device_0000000b, dev);
         __PRE_XEN_DOMCTL_READ(deassign_device, assign_device_0000000b, flag);
         switch (domctl->u.assign_device_0000000b.dev) {
         case VKI_XEN_DOMCTL_DEV_PCI:
            __PRE_XEN_DOMCTL_READ(deassign_device, assign_device_0000000b, u.pci);
            break;
         case VKI_XEN_DOMCTL_DEV_DT:
            __PRE_XEN_DOMCTL_READ(deassign_device, assign_device_0000000b, u.dt);
            PRE_MEM_READ("XEN_DOMTCL_assign_device.dt",
                          (Addr)domctl->u.assign_device_0000000b.u.dt.path.p,
                          domctl->u.assign_device_0000000b.u.dt.size);
            break;
         default:
            bad_subop(tid, layout, arrghs, status, flags,
                         "__HYPERVISOR_domctl_deassign_device dev",
                         domctl->u.assign_device_0000000b.dev);
            break;
         }
         break;
      }
      break;

   case VKI_XEN_DOMCTL_settscinfo:
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_00000007, info.tsc_mode);
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_00000007, info.gtsc_khz);
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_00000007, info.incarnation);
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_00000007, info.elapsed_nsec);
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_0000000b, tsc_mode);
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_0000000b, gtsc_khz);
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_0000000b, incarnation);
         __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info_0000000b, elapsed_nsec);
         break;
      }
      break;

   case VKI_XEN_DOMCTL_irq_permission:
      PRE_XEN_DOMCTL_READ(irq_permission, pirq);
      PRE_XEN_DOMCTL_READ(irq_permission, allow_access);
      break;

   case VKI_XEN_DOMCTL_iomem_permission:
      PRE_XEN_DOMCTL_READ(iomem_permission, first_mfn);
      PRE_XEN_DOMCTL_READ(iomem_permission, nr_mfns);
      PRE_XEN_DOMCTL_READ(iomem_permission, allow_access);
      break;

   case VKI_XEN_DOMCTL_ioport_permission:
      PRE_XEN_DOMCTL_READ(ioport_permission, first_port);
      PRE_XEN_DOMCTL_READ(ioport_permission, nr_ports);
      PRE_XEN_DOMCTL_READ(ioport_permission, allow_access);
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
         case VKI_XEN_SCHEDULER_RTDS:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.rtds.period);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.rtds.budget);
            break;
         case VKI_XEN_SCHEDULER_ARINC653:
            break;
         }
      }
      break;

   case VKI_XEN_DOMCTL_getvcpuaffinity:
      switch (domctl->interface_version) {
      case 0x00000007:
      case 0x00000008:
      case 0x00000009:
         __PRE_XEN_DOMCTL_READ(getvcpuaffinity, vcpuaffinity_00000009, vcpu);
         __PRE_XEN_DOMCTL_READ(getvcpuaffinity, vcpuaffinity_00000009, cpumap.nr_bits);
         break;
      case 0x0000000a:
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(getvcpuaffinity, vcpuaffinity_0000000a, vcpu);
         if (domctl->u.vcpuaffinity_0000000a.flags & VKI_XEN_VCPUAFFINITY_HARD)
            __PRE_XEN_DOMCTL_READ(
               setvcpuaffinity, vcpuaffinity_0000000a, cpumap_hard.nr_bits);
         if (domctl->u.vcpuaffinity_0000000a.flags & VKI_XEN_VCPUAFFINITY_SOFT)
            __PRE_XEN_DOMCTL_READ(
               setvcpuaffinity, vcpuaffinity_0000000a, cpumap_soft.nr_bits);
         break;
      }
      break;

   case VKI_XEN_DOMCTL_setvcpuaffinity:
      switch (domctl->interface_version) {
      case 0x00000007:
      case 0x00000008:
      case 0x00000009:
         __PRE_XEN_DOMCTL_READ(setvcpuaffinity, vcpuaffinity_00000009, vcpu);
         __PRE_XEN_DOMCTL_READ(setvcpuaffinity, vcpuaffinity_00000009, cpumap.nr_bits);
         PRE_MEM_READ("XEN_DOMCTL_setvcpuaffinity u.vcpuaffinity.cpumap.bitmap",
                      (Addr)domctl->u.vcpuaffinity_00000009.cpumap.bitmap.p,
                      domctl->u.vcpuaffinity_00000009.cpumap.nr_bits / 8);
         break;
      case 0x0000000a:
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(setvcpuaffinity, vcpuaffinity_0000000a, vcpu);
         __PRE_XEN_DOMCTL_READ(setvcpuaffinity, vcpuaffinity_0000000a, flags);
         if (domctl->u.vcpuaffinity_0000000a.flags & VKI_XEN_VCPUAFFINITY_HARD) {
            __PRE_XEN_DOMCTL_READ(
               setvcpuaffinity, vcpuaffinity_0000000a, cpumap_hard.nr_bits);
            PRE_MEM_READ(
               "XEN_DOMCTL_setvcpuaffinity u.vcpuaffinity.cpumap_hard.bitmap",
               (Addr)domctl->u.vcpuaffinity_0000000a.cpumap_hard.bitmap.p,
               domctl->u.vcpuaffinity_0000000a.cpumap_hard.nr_bits / 8);
         }
         if (domctl->u.vcpuaffinity_0000000a.flags & VKI_XEN_VCPUAFFINITY_SOFT) {
            __PRE_XEN_DOMCTL_READ(
               setvcpuaffinity, vcpuaffinity_0000000a, cpumap_soft.nr_bits);
            PRE_MEM_READ(
               "XEN_DOMCTL_setvcpuaffinity u.vcpuaffinity.cpumap_soft.bitmap",
               (Addr)domctl->u.vcpuaffinity_0000000a.cpumap_soft.bitmap.p,
               domctl->u.vcpuaffinity_0000000a.cpumap_soft.nr_bits / 8);
         }
      break;
      }
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

   case VKI_XEN_DOMCTL_pin_mem_cacheattr:
      PRE_XEN_DOMCTL_READ(pin_mem_cacheattr, start);
      PRE_XEN_DOMCTL_READ(pin_mem_cacheattr, end);
      PRE_XEN_DOMCTL_READ(pin_mem_cacheattr, type);
      break;

   case VKI_XEN_DOMCTL_get_ext_vcpucontext:
      switch (domctl->interface_version)
      {
      case 0x00000007:
      case 0x00000008:
         __PRE_XEN_DOMCTL_READ(get_ext_vcpucontext, ext_vcpucontext_00000008, vcpu);
         break;

      case 0x00000009:
      case 0x0000000a:
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(get_ext_vcpucontext, ext_vcpucontext_00000009, vcpu);
         break;

      default:
         VG_(dmsg)("WARNING: VKI_XEN_DOMCTL_get_ext_vcpucontext  domctl version %#"
                   PRIx32" not implemented\n", domctl->interface_version);
         SET_STATUS_Failure(VKI_EINVAL);
         break;
      }
      break;

   case VKI_XEN_DOMCTL_set_ext_vcpucontext:
       switch (domctl->interface_version)
       {
       case 0x00000007:
       case 0x00000008:
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008, vcpu);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008, size);
#if defined(__i386__) || defined(__x86_64__)
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                 syscall32_callback_eip);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                 sysenter_callback_eip);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                 syscall32_callback_cs);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                 sysenter_callback_cs);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                 syscall32_disables_events);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                 sysenter_disables_events);

           if ( domctl->u.ext_vcpucontext_00000008.size >=
                offsetof(struct vki_xen_domctl_ext_vcpucontext_00000008, mcg_cap) )
               __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000008,
                                     mcg_cap);
#endif
           break;

       case 0x00000009:
       case 0x0000000a:
       case 0x0000000b:
       case 0x0000000c:
       case 0x0000000d:
       case 0x0000000e:
       case 0x0000000f:
       case 0x00000010:
       case 0x00000011:
       case 0x00000012:
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009, vcpu);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009, size);
#if defined(__i386__) || defined(__x86_64__)
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                 syscall32_callback_eip);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                 sysenter_callback_eip);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                 syscall32_callback_cs);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                 sysenter_callback_cs);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                 syscall32_disables_events);
           __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                 sysenter_disables_events);

           if ( domctl->u.ext_vcpucontext_00000009.size >=
                offsetof(struct vki_xen_domctl_ext_vcpucontext_00000009, caps) )
           {
               __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                     caps);
               __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                     mci_ctl2_bank0);
               __PRE_XEN_DOMCTL_READ(set_ext_vcpucontext, ext_vcpucontext_00000009,
                                     mci_ctl2_bank1);
           }
#endif
	   break;

       default:
           VG_(dmsg)("WARNING: VKI_XEN_DOMCTL_set_ext_vcpucontext  domctl version %#"
                     PRIx32" not implemented\n", domctl->interface_version);
           SET_STATUS_Failure(VKI_EINVAL);
           break;
       }
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

   case VKI_XEN_DOMCTL_setvcpuextstate:
      __PRE_XEN_DOMCTL_READ(setvcpuextstate, vcpuextstate, vcpu);
      __PRE_XEN_DOMCTL_READ(setvcpuextstate, vcpuextstate, size);
      __PRE_XEN_DOMCTL_READ(setvcpuextstate, vcpuextstate, buffer);
      PRE_MEM_READ("XEN_DOMCTL_setvcpuextstate *u.vcpuextstate.buffer.p",
                   (Addr)domctl->u.vcpuextstate.buffer.p,
                   domctl->u.vcpuextstate.size);
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
       case VKI_XEN_DOMCTL_SHADOW_OP_GET_ALLOCATION:
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

       case VKI_XEN_DOMCTL_SHADOW_OP_SET_ALLOCATION:
           PRE_XEN_DOMCTL_READ(shadow_op, mb);
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

   case VKI_XEN_DOMCTL_set_access_required:
      PRE_XEN_DOMCTL_READ(access_required, access_required);
      break;

   case VKI_XEN_DOMCTL_mem_event_op:
   //case VKI_XEN_DOMCTL_vm_event_op: /* name change in 4.6 */
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __PRE_XEN_DOMCTL_READ(mem_event_op, mem_event_op_00000007, op);
         __PRE_XEN_DOMCTL_READ(mem_event_op, mem_event_op_00000007, mode);
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
         __PRE_XEN_DOMCTL_READ(vm_event_op, vm_event_op_0000000b, op);
         __PRE_XEN_DOMCTL_READ(vm_event_op, vm_event_op_0000000b, mode);
         break;
      case 0x00000012:
         __PRE_XEN_DOMCTL_READ(vm_event_op, vm_event_op_00000012, op);
         __PRE_XEN_DOMCTL_READ(vm_event_op, vm_event_op_00000012, mode);
         __PRE_XEN_DOMCTL_READ(vm_event_op, vm_event_op_00000012, u.enable);
         break;
      }
      break;

   case VKI_XEN_DOMCTL_debug_op:
      PRE_XEN_DOMCTL_READ(debug_op, op);
      PRE_XEN_DOMCTL_READ(debug_op, vcpu);
      break;

   case VKI_XEN_DOMCTL_get_vcpu_msrs:
      __PRE_XEN_DOMCTL_READ(get_vcpu_msrs, vcpu_msrs, vcpu);
      __PRE_XEN_DOMCTL_READ(get_vcpu_msrs, vcpu_msrs, msr_count);
      __PRE_XEN_DOMCTL_READ(get_vcpu_msrs, vcpu_msrs, msrs);
      break;

   case VKI_XEN_DOMCTL_set_vcpu_msrs:
      __PRE_XEN_DOMCTL_READ(set_vcpu_msrs, vcpu_msrs, vcpu);
      __PRE_XEN_DOMCTL_READ(set_vcpu_msrs, vcpu_msrs, msr_count);
      __PRE_XEN_DOMCTL_READ(set_vcpu_msrs, vcpu_msrs, msrs);
      PRE_MEM_READ("XEN_DOMCTL_set_vcpu_msrs *u.vcpu_msrs.msrs.p",
                   (Addr)domctl->u.vcpu_msrs.msrs.p,
                   sizeof(vki_xen_domctl_vcpu_msr_t) *
                   domctl->u.vcpu_msrs.msr_count);
      break;

   case VKI_XEN_DOMCTL_monitor_op:
      switch (domctl->interface_version) {
      case 0x000000b:
      case 0x000000c:
      case 0x000000d:
      case 0x000000e:
      case 0x000000f:
      case 0x0000010:
          if (domctl->u.monitor_op_0000000b.op == VKI_XEN_DOMCTL_MONITOR_OP_ENABLE ||
              domctl->u.monitor_op_0000000b.op == VKI_XEN_DOMCTL_MONITOR_OP_DISABLE) {
             switch (domctl->u.monitor_op_0000000b.event) {
             case VKI_XEN_DOMCTL_MONITOR_EVENT_WRITE_CTRLREG:
                __PRE_XEN_DOMCTL_READ(monitor_op, monitor_op_0000000b, u.mov_to_cr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_MOV_TO_MSR:
                __PRE_XEN_DOMCTL_READ(monitor_op, monitor_op_0000000b, u.mov_to_msr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_GUEST_REQUEST:
                __PRE_XEN_DOMCTL_READ(monitor_op, monitor_op_0000000b, u.guest_request);
                break;
             case VKI_XEN_DOMCTL_MONITOR_OP_GET_CAPABILITIES:
                break;
             }
          }

         break;
      case 0x0000011:
      case 0x0000012:
          if (domctl->u.monitor_op_00000011.op == VKI_XEN_DOMCTL_MONITOR_OP_ENABLE ||
              domctl->u.monitor_op_00000011.op == VKI_XEN_DOMCTL_MONITOR_OP_DISABLE) {
             switch (domctl->u.monitor_op_00000011.event) {
             case VKI_XEN_DOMCTL_MONITOR_EVENT_WRITE_CTRLREG:
                __PRE_XEN_DOMCTL_READ(monitor_op, monitor_op_00000011, u.mov_to_cr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_MOV_TO_MSR:
                __PRE_XEN_DOMCTL_READ(monitor_op, monitor_op_00000011, u.mov_to_msr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_GUEST_REQUEST:
                __PRE_XEN_DOMCTL_READ(monitor_op, monitor_op_00000011, u.guest_request);
                break;
             case VKI_XEN_DOMCTL_MONITOR_OP_GET_CAPABILITIES:
                break;
             }
          }

         break;
      }
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

   PRINT("__HYPERVISOR_hvm_op ( %ld, %#lx )", SARG1, ARG2);

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

   case VKI_XEN_HVMOP_set_pci_intx_level:
      PRE_XEN_HVMOP_READ(set_pci_intx_level, domid);
      PRE_XEN_HVMOP_READ(set_pci_intx_level, domain);
      PRE_XEN_HVMOP_READ(set_pci_intx_level, bus);
      PRE_XEN_HVMOP_READ(set_pci_intx_level, device);
      PRE_XEN_HVMOP_READ(set_pci_intx_level, level);
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

   case VKI_XEN_HVMOP_track_dirty_vram: {
      vki_xen_hvm_track_dirty_vram_t *Arg =
         (vki_xen_hvm_track_dirty_vram_t*)ARG2;
      PRE_XEN_HVMOP_READ(track_dirty_vram, domid);
      PRE_XEN_HVMOP_READ(track_dirty_vram, nr);
      if ( Arg->nr ) {
         PRE_XEN_HVMOP_READ(track_dirty_vram, first_pfn);
         PRE_XEN_HVMOP_READ(track_dirty_vram, dirty_bitmap);
      }
      break;
   }

   case VKI_XEN_HVMOP_set_mem_type:
      PRE_XEN_HVMOP_READ(set_mem_type, domid);
      PRE_XEN_HVMOP_READ(set_mem_type, hvmmem_type);
      PRE_XEN_HVMOP_READ(set_mem_type, nr);
      PRE_XEN_HVMOP_READ(set_mem_type, first_pfn);
      break;

   case VKI_XEN_HVMOP_set_mem_access:
      PRE_XEN_HVMOP_READ(set_mem_access, domid);
      PRE_XEN_HVMOP_READ(set_mem_access, hvmmem_access);
      PRE_XEN_HVMOP_READ(set_mem_access, first_pfn);
      /* if default access */
      if ( ((vki_xen_hvm_set_mem_access_t*)arg)->first_pfn != ~0ULL)
         PRE_XEN_HVMOP_READ(set_mem_access, nr);
      break;

   case VKI_XEN_HVMOP_get_mem_access:
      PRE_XEN_HVMOP_READ(get_mem_access, domid);
      PRE_XEN_HVMOP_READ(get_mem_access, pfn);

      PRE_MEM_WRITE("XEN_HVMOP_get_mem_access *hvmmem_access",
                    (Addr)&(((vki_xen_hvm_get_mem_access_t*)arg)->hvmmem_access),
                    sizeof(vki_uint16_t));
      break;

   case VKI_XEN_HVMOP_inject_trap:
      PRE_XEN_HVMOP_READ(inject_trap, domid);
      PRE_XEN_HVMOP_READ(inject_trap, vcpuid);
      PRE_XEN_HVMOP_READ(inject_trap, vector);
      PRE_XEN_HVMOP_READ(inject_trap, type);
      PRE_XEN_HVMOP_READ(inject_trap, error_code);
      PRE_XEN_HVMOP_READ(inject_trap, insn_len);
      PRE_XEN_HVMOP_READ(inject_trap, cr2);
      break;

   case VKI_XEN_HVMOP_altp2m: {
      vki_xen_hvm_altp2m_op_t *altp2m_op = (vki_xen_hvm_altp2m_op_t *)arg;

      PRE_XEN_HVMOP_READ(altp2m_op, version);
      PRE_XEN_HVMOP_READ(altp2m_op, cmd);
      PRE_XEN_HVMOP_READ(altp2m_op, domain);
      PRE_XEN_HVMOP_READ(altp2m_op, pad1);
      PRE_XEN_HVMOP_READ(altp2m_op, pad2);

      switch (altp2m_op->cmd) {
      case VKI_XEN_HVMOP_altp2m_get_domain_state:
      case VKI_XEN_HVMOP_altp2m_set_domain_state:
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.domain_state.state), sizeof(vki_uint8_t));
        break;
      case VKI_XEN_HVMOP_altp2m_create_p2m:
      case VKI_XEN_HVMOP_altp2m_destroy_p2m:
      case VKI_XEN_HVMOP_altp2m_switch_p2m:
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.view.view), sizeof(vki_uint16_t));
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.view.hvmmem_default_access), sizeof(vki_uint16_t));
        break;
      case VKI_XEN_HVMOP_altp2m_change_gfn:
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.change_gfn.view), sizeof(vki_uint16_t));
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.change_gfn.pad1), sizeof(vki_uint16_t));
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.change_gfn.pad2), sizeof(vki_uint32_t));
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.change_gfn.old_gfn), sizeof(vki_uint64_t));
        PRE_MEM_READ("XEN_HVMOP_altp2m_op", (Addr)&(altp2m_op->u.change_gfn.new_gfn), sizeof(vki_uint64_t));
        break;
      };

      break;
   }

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

    PRINT("__HYPERVISOR_tmem_op ( %u )", tmem->cmd);

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
                     (Addr)&tmem->pool_id, sizeof(tmem->pool_id));
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
   case VKI_XENMEM_access_op:
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

   case VKI_XENMEM_machphys_mfn_list:
   case VKI_XENMEM_machphys_compat_mfn_list: {
       struct vki_xen_machphys_mfn_list *arg =
           (struct vki_xen_machphys_mfn_list *)ARG2;
       POST_MEM_WRITE((Addr)&arg->nr_extents, sizeof(arg->nr_extents));
       POST_MEM_WRITE((Addr)arg->extent_start.p,
                      sizeof(vki_xen_pfn_t) * arg->nr_extents);
       break;
   }

   case VKI_XENMEM_memory_map:
   case VKI_XENMEM_machine_memory_map: {
      struct vki_xen_memory_map *arg =
         (struct vki_xen_memory_map *)ARG2;
      POST_MEM_WRITE(arg->nr_entries, sizeof(arg->nr_entries));
      POST_MEM_WRITE((Addr)arg->buffer.p,
                     arg->nr_entries * 20 /* size of an e820 entry */);
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

POST(xsm_op)
{
   /* XXX assuming flask, only actual XSM right now */
   struct vki_xen_flask_op *op = (struct vki_xen_flask_op *)ARG1;

   switch (op->interface_version) {
   case 0x00000001:
      break;
   default:
      return;
   }

#define POST_XEN_XSM_OP_WRITE(_xsm_op, _union, _field)        \
      POST_MEM_WRITE((Addr)&op->u._union._field,              \
                     sizeof(op->u._union._field))

   switch (op->cmd) {
   case VKI_FLASK_SID_TO_CONTEXT:
      POST_XEN_XSM_OP_WRITE(SID_TO_CONTEXT, sid_context, size);
      POST_MEM_WRITE((Addr)op->u.sid_context.context.p,
                     op->u.sid_context.size);
   }
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

POST(sched_op)
{
   switch (ARG1) {
   case VKI_XEN_SCHEDOP_remote_shutdown:
      /* No outputs */
      break;
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

POST(physdev_op)
{
   int cmd = ARG1;

#define POST_XEN_PHYSDEVOP_WRITE(_op, _field)                   \
   POST_MEM_WRITE((Addr)&arg->_field, sizeof(arg->_field))

   switch (cmd) {
   case VKI_XEN_PHYSDEVOP_unmap_pirq:
      /* No outputs */
      break;

   case VKI_XEN_PHYSDEVOP_map_pirq: {
      struct vki_xen_physdev_map_pirq *arg =
         (struct vki_xen_physdev_map_pirq *)ARG2;
      if (arg->type == VKI_XEN_MAP_PIRQ_TYPE_MULTI_MSI)
         POST_XEN_PHYSDEVOP_WRITE("map_pirq", entry_nr);
      POST_XEN_PHYSDEVOP_WRITE("map_pirq", pirq);
      break;
   }
#undef POST_XEN_PHYSDEVOP_WRITE

   default:
      break;
   }
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
   case 0x0000000b:
   case 0x0000000c:
   case 0x0000000d:
   case 0x0000000e:
   case 0x0000000f:
   case 0x00000010:
   case 0x00000011:
   case 0x00000012:
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
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
	 POST_XEN_SYSCTL_WRITE(getdomaininfolist_0000000a, num_domains);
	 POST_MEM_WRITE((Addr)sysctl->u.getdomaininfolist_0000000a.buffer.p,
			sizeof(*sysctl->u.getdomaininfolist_0000000a.buffer.p)
			* sysctl->u.getdomaininfolist_0000000a.num_domains);
	 break;
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
	 POST_XEN_SYSCTL_WRITE(getdomaininfolist_00000010, num_domains);
	 POST_MEM_WRITE((Addr)sysctl->u.getdomaininfolist_00000010.buffer.p,
			sizeof(*sysctl->u.getdomaininfolist_00000010.buffer.p)
			* sysctl->u.getdomaininfolist_00000010.num_domains);
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
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
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
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, threads_per_core);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, cores_per_socket);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, nr_cpus);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, max_cpu_id);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, nr_nodes);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, max_node_id);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, cpu_khz);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, capabilities);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, total_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, free_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, scrub_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, outstanding_pages);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, max_mfn);
         POST_XEN_SYSCTL_WRITE(physinfo_00000010, hw_cap[8]);
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
   case 0x0000000a:
   case 0x0000000b:
   case 0x0000000c:
   case 0x0000000d:
   case 0x0000000e:
   case 0x0000000f:
   case 0x00000010:
   case 0x00000011:
   case 0x00000012:
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
   case VKI_XEN_DOMCTL_setvcpuextstate:
   case VKI_XEN_DOMCTL_set_address_size:
   case VKI_XEN_DOMCTL_test_assign_device:
   case VKI_XEN_DOMCTL_assign_device:
   case VKI_XEN_DOMCTL_deassign_device:
   case VKI_XEN_DOMCTL_settscinfo:
   case VKI_XEN_DOMCTL_irq_permission:
   case VKI_XEN_DOMCTL_iomem_permission:
   case VKI_XEN_DOMCTL_ioport_permission:
   case VKI_XEN_DOMCTL_hypercall_init:
   case VKI_XEN_DOMCTL_setvcpucontext:
   case VKI_XEN_DOMCTL_pin_mem_cacheattr:
   case VKI_XEN_DOMCTL_set_ext_vcpucontext:
   case VKI_XEN_DOMCTL_setnodeaffinity:
   case VKI_XEN_DOMCTL_set_cpuid:
   case VKI_XEN_DOMCTL_unpausedomain:
   case VKI_XEN_DOMCTL_sethvmcontext:
   case VKI_XEN_DOMCTL_debug_op:
   case VKI_XEN_DOMCTL_set_max_evtchn:
   case VKI_XEN_DOMCTL_cacheflush:
   case VKI_XEN_DOMCTL_resumedomain:
   case VKI_XEN_DOMCTL_set_vcpu_msrs:
   case VKI_XEN_DOMCTL_set_access_required:
      /* No output fields */
      break;

   case VKI_XEN_DOMCTL_max_vcpus:
      POST_XEN_DOMCTL_WRITE(max_vcpus, max);
      break;

   case VKI_XEN_DOMCTL_get_address_size:
      __POST_XEN_DOMCTL_WRITE(get_address_size, address_size, size);
      break;

   case VKI_XEN_DOMCTL_gettscinfo:
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __POST_XEN_DOMCTL_WRITE(gettscinfo, tsc_info_00000007, out_info);
         POST_MEM_WRITE((Addr)domctl->u.tsc_info_00000007.out_info.p,
                        sizeof(vki_xen_guest_tsc_info_t));
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         __POST_XEN_DOMCTL_WRITE(gettscinfo, tsc_info_0000000b, tsc_mode);
         __POST_XEN_DOMCTL_WRITE(gettscinfo, tsc_info_0000000b, gtsc_khz);
         __POST_XEN_DOMCTL_WRITE(gettscinfo, tsc_info_0000000b, incarnation);
         __POST_XEN_DOMCTL_WRITE(gettscinfo, tsc_info_0000000b, elapsed_nsec);
         break;
      }
      break;
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

   case VKI_XEN_DOMCTL_gethvmcontext_partial:
       switch (domctl->u.hvmcontext_partial_00000007.type) {
       case VKI_HVM_SAVE_CODE(CPU):
           if ( domctl->u.hvmcontext_partial_00000007.buffer.p )
                POST_MEM_WRITE((Addr)domctl->u.hvmcontext_partial_00000007.buffer.p,
                   VKI_HVM_SAVE_LENGTH(CPU));
           break;
       }
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
         case VKI_XEN_SCHEDULER_RTDS:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.rtds.period);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.rtds.budget);
            break;
         }
      }
      break;

   case VKI_XEN_DOMCTL_getvcpuaffinity:
   case VKI_XEN_DOMCTL_setvcpuaffinity: /* Writes back actual result */
      switch (domctl->interface_version) {
      case 0x00000007:
      case 0x00000008:
      case 0x00000009:
         POST_MEM_WRITE((Addr)domctl->u.vcpuaffinity_00000009.cpumap.bitmap.p,
                        domctl->u.vcpuaffinity_00000009.cpumap.nr_bits / 8);
         break;
      case 0x0000000a:
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
         if (domctl->u.vcpuaffinity_0000000a.flags & VKI_XEN_VCPUAFFINITY_HARD)
            POST_MEM_WRITE(
               (Addr)domctl->u.vcpuaffinity_0000000a.cpumap_hard.bitmap.p,
               domctl->u.vcpuaffinity_0000000a.cpumap_hard.nr_bits / 8);
         if (domctl->u.vcpuaffinity_0000000a.flags & VKI_XEN_VCPUAFFINITY_SOFT)
            POST_MEM_WRITE(
               (Addr)domctl->u.vcpuaffinity_0000000a.cpumap_soft.bitmap.p,
               domctl->u.vcpuaffinity_0000000a.cpumap_soft.nr_bits / 8);
      }
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
      case 0x0000000a:
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
      case 0x00000012:
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

   case VKI_XEN_DOMCTL_get_ext_vcpucontext:
       switch (domctl->interface_version)
       {
       case 0x00000007:
       case 0x00000008:
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008, size);
#if defined(__i386__) || defined(__x86_64__)
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   syscall32_callback_eip);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   sysenter_callback_eip);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   syscall32_callback_cs);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   sysenter_callback_cs);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   syscall32_disables_events);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   sysenter_disables_events);

           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000008,
                                   mcg_cap);
#endif
           break;

       case 0x00000009:
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009, size);
#if defined(__i386__) || defined(__x86_64__)
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   syscall32_callback_eip);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   sysenter_callback_eip);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   syscall32_callback_cs);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   sysenter_callback_cs);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   syscall32_disables_events);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   sysenter_disables_events);

           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   caps);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   mci_ctl2_bank0);
           __POST_XEN_DOMCTL_WRITE(get_ext_vcpucontext, ext_vcpucontext_00000009,
                                   mci_ctl2_bank1);
#endif
	   break;
       }
       break;


   case VKI_XEN_DOMCTL_getvcpuextstate:
      if (domctl->u.vcpuextstate.buffer.p)
         POST_MEM_WRITE((Addr)domctl->u.vcpuextstate.buffer.p,
                        domctl->u.vcpuextstate.size);
      break;

   case VKI_XEN_DOMCTL_shadow_op:
       switch(domctl->u.shadow_op.op)
       {
       case VKI_XEN_DOMCTL_SHADOW_OP_OFF:
       case VKI_XEN_DOMCTL_SHADOW_OP_SET_ALLOCATION:
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

       case VKI_XEN_DOMCTL_SHADOW_OP_GET_ALLOCATION:
           POST_XEN_DOMCTL_WRITE(shadow_op, mb);
           break;

       default:
           break;
       }
       break;
   case VKI_XEN_DOMCTL_get_vcpu_msrs:
      if (domctl->u.vcpu_msrs.msrs.p)
         POST_MEM_WRITE((Addr)domctl->u.vcpu_msrs.msrs.p,
                        sizeof(vki_xen_domctl_vcpu_msr_t) *
                        domctl->u.vcpu_msrs.msr_count);
      break;

   case VKI_XEN_DOMCTL_mem_event_op:
   //case VKI_XEN_DOMCTL_vm_event_op: /* name change in 4.6 */
      switch (domctl->interface_version) {
      case 0x00000007: /* pre-4.6 */
      case 0x00000008:
      case 0x00000009:
      case 0x0000000a:
         __POST_XEN_DOMCTL_WRITE(mem_event_op, mem_event_op_00000007, port);
         break;
      case 0x0000000b:
      case 0x0000000c:
      case 0x0000000d:
      case 0x0000000e:
      case 0x0000000f:
      case 0x00000010:
      case 0x00000011:
         __POST_XEN_DOMCTL_WRITE(vm_event_op, vm_event_op_0000000b, port);
         break;
      case 0x00000012:
         __POST_XEN_DOMCTL_WRITE(vm_event_op, vm_event_op_00000012, u.enable.port);
         break;
      }
      break;

   case VKI_XEN_DOMCTL_monitor_op:
      switch (domctl->interface_version) {
      case 0x000000b:
          if (domctl->u.monitor_op_0000000b.op == VKI_XEN_DOMCTL_MONITOR_OP_GET_CAPABILITIES) {
             switch(domctl->u.monitor_op_0000000b.event) {
             case VKI_XEN_DOMCTL_MONITOR_EVENT_WRITE_CTRLREG:
                __POST_XEN_DOMCTL_WRITE(monitor_op, monitor_op_0000000b, u.mov_to_cr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_MOV_TO_MSR:
                __POST_XEN_DOMCTL_WRITE(monitor_op, monitor_op_0000000b, u.mov_to_msr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_GUEST_REQUEST:
                __POST_XEN_DOMCTL_WRITE(monitor_op, monitor_op_0000000b, u.guest_request);
                break;
             }
          }

         break;
      case 0x0000011:
          if (domctl->u.monitor_op_00000011.op == VKI_XEN_DOMCTL_MONITOR_OP_GET_CAPABILITIES) {
             switch(domctl->u.monitor_op_00000011.event) {
             case VKI_XEN_DOMCTL_MONITOR_EVENT_WRITE_CTRLREG:
                __POST_XEN_DOMCTL_WRITE(monitor_op, monitor_op_00000011, u.mov_to_cr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_MOV_TO_MSR:
                __POST_XEN_DOMCTL_WRITE(monitor_op, monitor_op_00000011, u.mov_to_msr);
                break;
             case VKI_XEN_DOMCTL_MONITOR_EVENT_GUEST_REQUEST:
                __POST_XEN_DOMCTL_WRITE(monitor_op, monitor_op_00000011, u.guest_request);
                break;
             }
          }

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
      __POST_XEN_HVMOP_WRITE(_hvm_op, vki_xen_hvm_ ## _hvm_op ## _t, _field)

   switch (op) {
   case VKI_XEN_HVMOP_set_param:
   case VKI_XEN_HVMOP_set_pci_intx_level:
   case VKI_XEN_HVMOP_set_isa_irq_level:
   case VKI_XEN_HVMOP_set_pci_link_route:
   case VKI_XEN_HVMOP_set_mem_type:
   case VKI_XEN_HVMOP_set_mem_access:
   case VKI_XEN_HVMOP_inject_trap:
      /* No output parameters */
      break;

   case VKI_XEN_HVMOP_get_param:
      __POST_XEN_HVMOP_WRITE(get_param, struct vki_xen_hvm_param, value);
      break;

   case VKI_XEN_HVMOP_get_mem_access:
      POST_XEN_HVMOP_WRITE(get_mem_access, hvmmem_access);
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
   HYPXY(__VKI_XEN_xsm_op,                  xsm_op,            1), // 27
   //    __VKI_XEN_nmi_op                                          // 28
   HYPXY(__VKI_XEN_sched_op,                sched_op,          2), // 29

   //    __VKI_XEN_callback_op                                     // 30
   //    __VKI_XEN_xenoprof_op                                     // 31
   HYPXY(__VKI_XEN_event_channel_op,        evtchn_op,         2), // 32
   HYPXY(__VKI_XEN_physdev_op,              physdev_op,        2), // 33
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
      VG_SYSNUM_STRING(args->sysno));
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
