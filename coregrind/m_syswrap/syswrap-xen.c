
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

#include <stdint.h>

#define __XEN_TOOLS__

#include <xen/xen.h>
#include <xen/sysctl.h>
#include <xen/domctl.h>
#include <xen/memory.h>
#include <xen/event_channel.h>
#include <xen/version.h>

#include <xen/hvm/hvm_op.h>

#define PRE(name) static DEFN_PRE_TEMPLATE(xen, name)
#define POST(name) static DEFN_POST_TEMPLATE(xen, name)

static void bad_subop ( ThreadId              tid,
                        SyscallArgLayout*     layout,
                        /*MOD*/SyscallArgs*   args,
                        /*OUT*/SyscallStatus* status,
                        /*OUT*/UWord*         flags,
                        const char*           hypercall,
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
   case XENMEM_set_memory_map: {
      xen_foreign_memory_map_t *arg =
         (xen_foreign_memory_map_t *)(unsigned int)ARG2;
      PRE_MEM_READ("XENMEM_set_memory_map",
                   (Addr)&arg->domid, sizeof(arg->domid));
      PRE_MEM_READ("XENMEM_set_memory_map",
                   (Addr)&arg->map, sizeof(arg->map));
      break;
   }
   case XENMEM_increase_reservation:
   case XENMEM_decrease_reservation:
   case XENMEM_populate_physmap: {
      struct xen_memory_reservation *memory_reservation =
         (struct xen_memory_reservation *)(unsigned int)ARG2;
      char *which;

      switch (ARG1) {
      case XENMEM_increase_reservation:
         which = "XENMEM_increase_reservation";
         break;
      case XENMEM_decrease_reservation:
         which = "XENMEM_decrease_reservation";
         PRE_MEM_READ(which,
                      (Addr)memory_reservation->extent_start.p,
                      sizeof(xen_pfn_t) * memory_reservation->nr_extents);
      case XENMEM_populate_physmap:
         which = "XENMEM_populate_physmap";
         PRE_MEM_READ(which,
                      (Addr)memory_reservation->extent_start.p,
                      sizeof(xen_pfn_t) * memory_reservation->nr_extents);
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

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_memory_op", ARG1);
      break;
   }
}

PRE(mmuext_op)
{
   mmuext_op_t *ops = (void *)(unsigned int)ARG1;
   unsigned int i, nr = ARG2;


   for (i=0; i<nr; i++) {
      mmuext_op_t *op = ops + i;
      PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP",
                   (Addr)&op->cmd, sizeof(op->cmd));
      switch(op->cmd) {
      case MMUEXT_PIN_L1_TABLE:
      case MMUEXT_PIN_L2_TABLE:
      case MMUEXT_PIN_L3_TABLE:
      case MMUEXT_PIN_L4_TABLE:
      case MMUEXT_UNPIN_TABLE:
      case MMUEXT_NEW_BASEPTR:
      case MMUEXT_CLEAR_PAGE:
      case MMUEXT_COPY_PAGE:
      case MMUEXT_MARK_SUPER:
      case MMUEXT_UNMARK_SUPER:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg1.mfn",
                      (Addr)&op->arg1.mfn,
                      sizeof(op->arg1.mfn));
         break;

      case MMUEXT_INVLPG_LOCAL:
      case MMUEXT_INVLPG_ALL:
      case MMUEXT_SET_LDT:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg1.mfn",
                      (Addr)&op->arg1.linear_addr,
                      sizeof(op->arg1.linear_addr));
         break;

      case MMUEXT_TLB_FLUSH_LOCAL:
      case MMUEXT_TLB_FLUSH_MULTI:
      case MMUEXT_INVLPG_MULTI:
      case MMUEXT_TLB_FLUSH_ALL:
      case MMUEXT_FLUSH_CACHE:
      case MMUEXT_NEW_USER_BASEPTR:
      case MMUEXT_FLUSH_CACHE_GLOBAL:
         /* None */
         break;
      }

      switch(op->cmd) {
      case MMUEXT_SET_LDT:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg2.nr_ents",
                      (Addr)&op->arg2.nr_ents,
                      sizeof(op->arg2.nr_ents));
         break;

      case MMUEXT_TLB_FLUSH_MULTI:
      case MMUEXT_INVLPG_MULTI:
         /* How many??? */
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg2.vcpumask",
                      (Addr)&op->arg2.vcpumask,
                      sizeof(op->arg2.vcpumask));
         break;

      case MMUEXT_COPY_PAGE:
         PRE_MEM_READ("__HYPERVISOR_MMUEXT_OP arg2.src_mfn",
                      (Addr)&op->arg2.src_mfn,
                      sizeof(op->arg2.src_mfn));
         break;

      case MMUEXT_PIN_L1_TABLE:
      case MMUEXT_PIN_L2_TABLE:
      case MMUEXT_PIN_L3_TABLE:
      case MMUEXT_PIN_L4_TABLE:
      case MMUEXT_UNPIN_TABLE:
      case MMUEXT_NEW_BASEPTR:
      case MMUEXT_TLB_FLUSH_LOCAL:
      case MMUEXT_INVLPG_LOCAL:
      case MMUEXT_TLB_FLUSH_ALL:
      case MMUEXT_INVLPG_ALL:
      case MMUEXT_FLUSH_CACHE:
      case MMUEXT_NEW_USER_BASEPTR:
      case MMUEXT_CLEAR_PAGE:
      case MMUEXT_FLUSH_CACHE_GLOBAL:
      case MMUEXT_MARK_SUPER:
      case MMUEXT_UNMARK_SUPER:
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
   case EVTCHNOP_alloc_unbound: {
      struct evtchn_alloc_unbound *alloc_unbound = arg;
      PRE_MEM_READ("EVTCHNOP_alloc_unbound",
                   (Addr)&alloc_unbound->dom, sizeof(alloc_unbound->dom));
      PRE_MEM_READ("EVTCHNOP_alloc_unbound",
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
                 ARG1, (void *)(unsigned int)ARG2, 0);
}

PRE(evtchn_op_compat)
{
   struct evtchn_op *evtchn = (struct evtchn_op *)(unsigned int)ARG1;
   PRE_MEM_READ("__HYPERVISOR_event_channel_op_compat",
                ARG1, sizeof(*evtchn));

   pre_evtchn_op(tid, layout, arrghs, status, flags,
                 evtchn->cmd, &evtchn->u, 1);
}

PRE(xen_version)
{
   PRINT("__HYPERVISOR_xen_version ( %ld, %lx )", ARG1, ARG2);

   switch (ARG1) {
   case XENVER_version:
   case XENVER_extraversion:
   case XENVER_compile_info:
   case XENVER_capabilities:
   case XENVER_changeset:
   case XENVER_platform_parameters:
   case XENVER_get_features:
   case XENVER_pagesize:
   case XENVER_guest_handle:
   case XENVER_commandline:
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
   case GNTTABOP_setup_table: {
      struct gnttab_setup_table *gst = (void *)(intptr_t)ARG2;
      PRE_MEM_READ("GNTTABOP_setup_table", (Addr)&gst->dom, sizeof(gst->dom));
      PRE_MEM_READ("GNTTABOP_setup_table",
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
   struct xen_sysctl *sysctl = (struct xen_sysctl *)(unsigned int)ARG1;

   PRINT("__HYPERVISOR_sysctl ( %d )", sysctl->cmd);

   /*
    * Common part of xen_sysctl:
    *    uint32_t cmd;
    *    uint32_t interface_version;
    */
   PRE_MEM_READ("__HYPERVISOR_sysctl", ARG1,
                sizeof(uint32_t) + sizeof(uint32_t));

   if (!sysctl || sysctl->interface_version != XEN_SYSCTL_INTERFACE_VERSION)
      /* BUG ? */
      return;

#define __PRE_XEN_SYSCTL_READ(_sysctl, _union, _field)  \
      PRE_MEM_READ("XEN_SYSCTL_" # _sysctl,             \
                   (Addr)&sysctl->u._union._field,      \
                   sizeof(sysctl->u._union._field))
#define PRE_XEN_SYSCTL_READ(_sysctl, _field) \
      __PRE_XEN_SYSCTL_READ(_sysctl, _sysctl, _field)

   switch (sysctl->cmd) {
   case XEN_SYSCTL_getdomaininfolist:
      PRE_XEN_SYSCTL_READ(getdomaininfolist, first_domain);
      PRE_XEN_SYSCTL_READ(getdomaininfolist, max_domains);
      PRE_XEN_SYSCTL_READ(getdomaininfolist, buffer);
      break;

   case XEN_SYSCTL_cpupool_op:
      PRE_XEN_SYSCTL_READ(cpupool_op, op);

      switch(sysctl->u.cpupool_op.op) {
      case XEN_SYSCTL_CPUPOOL_OP_CREATE:
      case XEN_SYSCTL_CPUPOOL_OP_DESTROY:
      case XEN_SYSCTL_CPUPOOL_OP_INFO:
      case XEN_SYSCTL_CPUPOOL_OP_ADDCPU:
      case XEN_SYSCTL_CPUPOOL_OP_RMCPU:
      case XEN_SYSCTL_CPUPOOL_OP_MOVEDOMAIN:
         PRE_XEN_SYSCTL_READ(cpupool_op, cpupool_id);
      }

      if (sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_CREATE)
         PRE_XEN_SYSCTL_READ(cpupool_op, sched_id);

      if (sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_MOVEDOMAIN)
         PRE_XEN_SYSCTL_READ(cpupool_op, domid);

      if (sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_ADDCPU ||
          sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_RMCPU)
         PRE_XEN_SYSCTL_READ(cpupool_op, cpu);

      break;

   case XEN_SYSCTL_physinfo:
      /* No input params */
      break;

   case XEN_SYSCTL_topologyinfo:
      PRE_XEN_SYSCTL_READ(topologyinfo, max_cpu_index);
      PRE_XEN_SYSCTL_READ(topologyinfo, cpu_to_core);
      PRE_XEN_SYSCTL_READ(topologyinfo, cpu_to_socket);
      PRE_XEN_SYSCTL_READ(topologyinfo, cpu_to_node);
      break;

   case XEN_SYSCTL_numainfo:
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
   struct xen_domctl *domctl = (struct xen_domctl *)(unsigned int)ARG1;

   PRINT("__HYPERVISOR_domctl ( %d ) on dom%d", domctl->cmd, domctl->domain);

   /*
    * Common part of xen_domctl:
    *    uint32_t cmd;
    *    uint32_t interface_version;
    *    domid_t  domain;
    */
   PRE_MEM_READ("__HYPERVISOR_domctl", ARG1,
                sizeof(uint32_t) + sizeof(uint32_t) + sizeof(domid_t));

   if (!domctl || domctl->interface_version != XEN_DOMCTL_INTERFACE_VERSION)
      /* BUG ? */
      return;

#define __PRE_XEN_DOMCTL_READ(_domctl, _union, _field)  \
      PRE_MEM_READ("XEN_DOMCTL_" # _domctl,             \
                   (Addr)&domctl->u._union._field,      \
                   sizeof(domctl->u._union._field))
#define PRE_XEN_DOMCTL_READ(_domctl, _field) \
      __PRE_XEN_DOMCTL_READ(_domctl, _domctl, _field)

   switch (domctl->cmd) {
   case XEN_DOMCTL_destroydomain:
   case XEN_DOMCTL_pausedomain:
   case XEN_DOMCTL_max_vcpus:
   case XEN_DOMCTL_get_address_size:
   case XEN_DOMCTL_gettscinfo:
   case XEN_DOMCTL_getdomaininfo:
   case XEN_DOMCTL_unpausedomain:
      /* No input fields. */
      break;

   case XEN_DOMCTL_createdomain:
      PRE_XEN_DOMCTL_READ(createdomain, ssidref);
      PRE_XEN_DOMCTL_READ(createdomain, handle);
      PRE_XEN_DOMCTL_READ(createdomain, flags);
      break;

   case XEN_DOMCTL_max_mem:
      PRE_XEN_DOMCTL_READ(max_mem, max_memkb);
      break;

   case XEN_DOMCTL_set_address_size:
      __PRE_XEN_DOMCTL_READ(set_address_size, address_size, size);
      break;

   case XEN_DOMCTL_settscinfo:
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.tsc_mode);
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.gtsc_khz);
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.incarnation);
      __PRE_XEN_DOMCTL_READ(settscinfo, tsc_info, info.elapsed_nsec);
      break;

   case XEN_DOMCTL_hypercall_init:
      PRE_XEN_DOMCTL_READ(hypercall_init, gmfn);
      break;

   case XEN_DOMCTL_getvcpuinfo:
      PRE_XEN_DOMCTL_READ(getvcpuinfo, vcpu);
      break;

   case XEN_DOMCTL_scheduler_op:
      PRE_XEN_DOMCTL_READ(scheduler_op, sched_id);
      PRE_XEN_DOMCTL_READ(scheduler_op, cmd);
      if ( domctl->u.scheduler_op.cmd == XEN_DOMCTL_SCHEDOP_putinfo ) {
         switch(domctl->u.scheduler_op.sched_id) {
         case XEN_SCHEDULER_SEDF:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.period);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.slice);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.latency);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.extratime);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.sedf.weight);
            break;
         case XEN_SCHEDULER_CREDIT:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.credit.weight);
            PRE_XEN_DOMCTL_READ(scheduler_op, u.credit.cap);
            break;
         case XEN_SCHEDULER_CREDIT2:
            PRE_XEN_DOMCTL_READ(scheduler_op, u.credit2.weight);
            break;
         case XEN_SCHEDULER_ARINC653:
            break;
         }
      }
      break;

   case XEN_DOMCTL_getvcpuaffinity:
      __PRE_XEN_DOMCTL_READ(getvcpuaffinity, vcpuaffinity, vcpu);
      break;

   case XEN_DOMCTL_setvcpuaffinity:
      __PRE_XEN_DOMCTL_READ(setvcpuaffinity, vcpuaffinity, vcpu);
      PRE_MEM_READ("XEN_DOMCTL_setvcpuaffinity",
                   (Addr)domctl->u.vcpuaffinity.cpumap.bitmap.p,
                   domctl->u.vcpuaffinity.cpumap.nr_cpus / 8);
      break;

   case XEN_DOMCTL_getvcpucontext:
      __PRE_XEN_DOMCTL_READ(getvcpucontext, vcpucontext, vcpu);
      break;

   case XEN_DOMCTL_setvcpucontext:
      __PRE_XEN_DOMCTL_READ(setvcpucontext, vcpucontext, vcpu);
      __PRE_XEN_DOMCTL_READ(setvcpucontext, vcpucontext, ctxt.p);
      break;

   case XEN_DOMCTL_set_cpuid:
      PRE_MEM_READ("XEN_DOMCTL_set_cpuid",
                   (Addr)&domctl->u.cpuid, sizeof(domctl->u.cpuid));
      break;

   case XEN_DOMCTL_getvcpuextstate:
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, vcpu);
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, xfeature_mask);
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, size);
      __PRE_XEN_DOMCTL_READ(getvcpuextstate, vcpuextstate, buffer);
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
   PRE_MEM_READ("XEN_HVMOP_" # _hvm_op,                 \
                (Addr)&((_type*)arg)->_field,           \
                sizeof(((_type*)arg)->_field))
#define PRE_XEN_HVMOP_READ(_hvm_op, _field)                             \
   __PRE_XEN_HVMOP_READ(_hvm_op, "xen_hvm_" # _hvm_op "_t", _field)

   switch (op) {
   case HVMOP_set_param:
      __PRE_XEN_HVMOP_READ(set_param, xen_hvm_param_t, domid);
      __PRE_XEN_HVMOP_READ(set_param, xen_hvm_param_t, index);
      __PRE_XEN_HVMOP_READ(set_param, xen_hvm_param_t, value);
      break;

   case HVMOP_get_param:
      __PRE_XEN_HVMOP_READ(get_param, xen_hvm_param_t, domid);
      __PRE_XEN_HVMOP_READ(get_param, xen_hvm_param_t, index);
      break;

   default:
      bad_subop(tid, layout, arrghs, status, flags,
                "__HYPERVISOR_hvm_op", op);
      break;
   }
#undef __PRE_XEN_HVMOP_READ
#undef PRE_XEN_HVMOP_READ
}

POST(memory_op)
{
   switch (ARG1) {
   case XENMEM_set_memory_map:
   case XENMEM_decrease_reservation:
      /* No outputs */
      break;
   case XENMEM_increase_reservation:
   case XENMEM_populate_physmap: {
      struct xen_memory_reservation *memory_reservation =
         (struct xen_memory_reservation *)(unsigned int)ARG2;

      POST_MEM_WRITE((Addr)memory_reservation->extent_start.p,
                     sizeof(xen_pfn_t) * memory_reservation->nr_extents);
      break;
   }
   }
}

POST(mmuext_op)
{
   unsigned int *pdone = (void *)(unsigned int)ARG3;
   /* simplistic */
   POST_MEM_WRITE((Addr)pdone, sizeof(*pdone));
}

static void post_evtchn_op(ThreadId tid, __vki_u32 cmd, void *arg, int compat)
{
   switch (cmd) {
   case EVTCHNOP_alloc_unbound: {
      struct evtchn_alloc_unbound *alloc_unbound = arg;
      POST_MEM_WRITE((Addr)&alloc_unbound->port, sizeof(alloc_unbound->port));
      break;
   }
   }
}

POST(evtchn_op)
{
   post_evtchn_op(tid, ARG1, (void *)(unsigned int)ARG2, 0);
}

POST(evtchn_op_compat)
{
   struct evtchn_op *evtchn = (struct evtchn_op *)(unsigned int)ARG1;
   post_evtchn_op(tid, evtchn->cmd, &evtchn->u, 1);
}

POST(xen_version)
{
   switch (ARG1) {
   case XENVER_version:
      /* No outputs */
      break;
   case XENVER_extraversion:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_extraversion_t));
      break;
   case XENVER_compile_info:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_compile_info_t));
      break;
   case XENVER_capabilities:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_capabilities_info_t));
      break;
   case XENVER_changeset:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_changeset_info_t));
      break;
   case XENVER_platform_parameters:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_platform_parameters_t));
      break;
   case XENVER_get_features:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_feature_info_t));
      break;
   case XENVER_pagesize:
      /* No outputs */
      break;
   case XENVER_guest_handle:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_domain_handle_t));
      break;
   case XENVER_commandline:
      POST_MEM_WRITE((Addr)ARG2, sizeof(xen_commandline_t));
      break;
   }
}

POST(grant_table_op)
{
   switch (ARG1) {
   case GNTTABOP_setup_table: {
      struct gnttab_setup_table *gst = (void *)(uintptr_t)ARG2;
      PRE_MEM_WRITE("GNTTABOP_setup_table",
                    (Addr)&gst->status, sizeof(gst->status));
      PRE_MEM_WRITE("GNTTABOP_setup_table",
                    (Addr)gst->frame_list.p,
                    sizeof(*gst->frame_list.p) & gst->nr_frames);
      break;
   }
   }
}

POST(sysctl)
{
   struct xen_sysctl *sysctl = (struct xen_sysctl *)(unsigned int)ARG1;

   if (!sysctl || sysctl->interface_version != XEN_SYSCTL_INTERFACE_VERSION)
      return;

#define __POST_XEN_SYSCTL_WRITE(_sysctl, _union, _field)        \
      POST_MEM_WRITE((Addr)&sysctl->u._union._field,            \
                     sizeof(sysctl->u._union._field))
#define POST_XEN_SYSCTL_WRITE(_sysctl, _field) \
      __POST_XEN_SYSCTL_WRITE(_sysctl, _sysctl, _field)

   switch (sysctl->cmd) {
   case XEN_SYSCTL_getdomaininfolist:
      POST_XEN_SYSCTL_WRITE(getdomaininfolist, num_domains);
      POST_MEM_WRITE((Addr)sysctl->u.getdomaininfolist.buffer.p,
                     sizeof(xen_domctl_getdomaininfo_t)
                     * sysctl->u.getdomaininfolist.num_domains);
      break;

   case XEN_SYSCTL_cpupool_op:
      if (sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_CREATE ||
          sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_INFO)
         POST_XEN_SYSCTL_WRITE(cpupool_op, cpupool_id);
      if (sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_INFO) {
         POST_XEN_SYSCTL_WRITE(cpupool_op, sched_id);
         POST_XEN_SYSCTL_WRITE(cpupool_op, n_dom);
      }
      if (sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_INFO ||
          sysctl->u.cpupool_op.op == XEN_SYSCTL_CPUPOOL_OP_FREEINFO)
         POST_XEN_SYSCTL_WRITE(cpupool_op, cpumap);
      break;

   case XEN_SYSCTL_physinfo:
      POST_XEN_SYSCTL_WRITE(physinfo, threads_per_core);
      POST_XEN_SYSCTL_WRITE(physinfo, cores_per_socket);
      POST_XEN_SYSCTL_WRITE(physinfo, nr_cpus);
      POST_XEN_SYSCTL_WRITE(physinfo, max_cpu_id);
      POST_XEN_SYSCTL_WRITE(physinfo, nr_nodes);
      POST_XEN_SYSCTL_WRITE(physinfo, max_node_id);
      POST_XEN_SYSCTL_WRITE(physinfo, cpu_khz);
      POST_XEN_SYSCTL_WRITE(physinfo, total_pages);
      POST_XEN_SYSCTL_WRITE(physinfo, free_pages);
      POST_XEN_SYSCTL_WRITE(physinfo, scrub_pages);
      POST_XEN_SYSCTL_WRITE(physinfo, hw_cap[8]);
      POST_XEN_SYSCTL_WRITE(physinfo, capabilities);
      break;

   case XEN_SYSCTL_topologyinfo:
      POST_XEN_SYSCTL_WRITE(topologyinfo, max_cpu_index);
      POST_MEM_WRITE((Addr)sysctl->u.topologyinfo.cpu_to_core.p,
                     sizeof(uint32_t) * sysctl->u.topologyinfo.max_cpu_index);
      POST_MEM_WRITE((Addr)sysctl->u.topologyinfo.cpu_to_socket.p,
                     sizeof(uint32_t) * sysctl->u.topologyinfo.max_cpu_index);
      POST_MEM_WRITE((Addr)sysctl->u.topologyinfo.cpu_to_node.p,
                     sizeof(uint32_t) * sysctl->u.topologyinfo.max_cpu_index);
      break;

   case XEN_SYSCTL_numainfo:
      POST_XEN_SYSCTL_WRITE(numainfo, max_node_index);
      POST_MEM_WRITE((Addr)sysctl->u.numainfo.node_to_memsize.p,
                     sizeof(uint64_t) * sysctl->u.numainfo.max_node_index);
      POST_MEM_WRITE((Addr)sysctl->u.numainfo.node_to_memfree.p,
                     sizeof(uint64_t) * sysctl->u.numainfo.max_node_index);
      POST_MEM_WRITE((Addr)sysctl->u.numainfo.node_to_node_distance.p,
                     sizeof(uint32_t) * sysctl->u.numainfo.max_node_index);
   }
#undef POST_XEN_SYSCTL_WRITE
#undef __POST_XEN_SYSCTL_WRITE
}

POST(domctl){
   struct xen_domctl *domctl = (struct xen_domctl *)(unsigned int)ARG1;

   if (!domctl || domctl->interface_version != XEN_DOMCTL_INTERFACE_VERSION)
      return;

#define __POST_XEN_DOMCTL_WRITE(_domctl, _union, _field)        \
   POST_MEM_WRITE((Addr)&domctl->u._union._field,               \
                  sizeof(domctl->u._union._field));
#define POST_XEN_DOMCTL_WRITE(_domctl, _field)          \
   __POST_XEN_DOMCTL_WRITE(_domctl, _domctl, _field)

   switch (domctl->cmd) {
   case XEN_DOMCTL_createdomain:
   case XEN_DOMCTL_destroydomain:
   case XEN_DOMCTL_pausedomain:
   case XEN_DOMCTL_max_mem:
   case XEN_DOMCTL_set_address_size:
   case XEN_DOMCTL_settscinfo:
   case XEN_DOMCTL_hypercall_init:
   case XEN_DOMCTL_setvcpuaffinity:
   case XEN_DOMCTL_setvcpucontext:
   case XEN_DOMCTL_set_cpuid:
   case XEN_DOMCTL_unpausedomain:
      /* No output fields */
      break;

   case XEN_DOMCTL_max_vcpus:
      POST_XEN_DOMCTL_WRITE(max_vcpus, max);

   case XEN_DOMCTL_get_address_size:
      __POST_XEN_DOMCTL_WRITE(get_address_size, address_size, size);
      break;

   case XEN_DOMCTL_gettscinfo:
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.tsc_mode);
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.gtsc_khz);
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.incarnation);
      __POST_XEN_DOMCTL_WRITE(settscinfo, tsc_info, info.elapsed_nsec);
      break;

   case XEN_DOMCTL_getvcpuinfo:
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, online);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, blocked);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, running);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, cpu_time);
      POST_XEN_DOMCTL_WRITE(getvcpuinfo, cpu);
      break;

   case XEN_DOMCTL_scheduler_op:
      if ( domctl->u.scheduler_op.cmd == XEN_DOMCTL_SCHEDOP_getinfo ) {
         switch(domctl->u.scheduler_op.sched_id) {
         case XEN_SCHEDULER_SEDF:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.period);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.slice);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.latency);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.extratime);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.sedf.weight);
            break;
         case XEN_SCHEDULER_CREDIT:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.credit.weight);
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.credit.cap);
            break;
         case XEN_SCHEDULER_CREDIT2:
            POST_XEN_DOMCTL_WRITE(scheduler_op, u.credit2.weight);
            break;
         case XEN_SCHEDULER_ARINC653:
            break;
         }
      }
      break;

   case XEN_DOMCTL_getvcpuaffinity:
      POST_MEM_WRITE((Addr)domctl->u.vcpuaffinity.cpumap.bitmap.p,
                     domctl->u.vcpuaffinity.cpumap.nr_cpus / 8);
      break;

   case XEN_DOMCTL_getdomaininfo:
      POST_XEN_DOMCTL_WRITE(getdomaininfo, domain);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, flags);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, tot_pages);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, max_pages);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, shr_pages);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, shared_info_frame);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, cpu_time);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, nr_online_vcpus);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, max_vcpu_id);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, ssidref);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, handle);
      POST_XEN_DOMCTL_WRITE(getdomaininfo, cpupool);
      break;

   case XEN_DOMCTL_getvcpucontext:
      __POST_XEN_DOMCTL_WRITE(getvcpucontext, vcpucontext, ctxt.p);
      break;

   case XEN_DOMCTL_getvcpuextstate:
      __POST_XEN_DOMCTL_WRITE(getvcpuextstate, vcpuextstate, xfeature_mask);
      __POST_XEN_DOMCTL_WRITE(getvcpuextstate, vcpuextstate, size);
      POST_MEM_WRITE((Addr)domctl->u.vcpuextstate.buffer.p,
                     domctl->u.vcpuextstate.size);
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
      __PRE_XEN_HVMOP_READ(_hvm_op, "xen_hvm_" # _hvm_op "_t", _field)

   switch (op) {
   case HVMOP_set_param:
      /* No output paramters */
      break;

   case HVMOP_get_param:
      __POST_XEN_HVMOP_WRITE(get_param, xen_hvm_param_t, value);
      break;
   }
#undef __POST_XEN_HVMOP_WRITE
#undef POST_XEN_HVMOP_WRITE
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
   //    __HYPERVISOR_set_trap_table                                  // 0
   //    __HYPERVISOR_mmu_update                                      // 1
   //    __HYPERVISOR_set_gdt                                         // 2
   //    __HYPERVISOR_stack_switch                                    // 3
   //    __HYPERVISOR_set_callbacks                                   // 4

   //    __HYPERVISOR_fpu_taskswitch                                  // 5
   //    __HYPERVISOR_sched_op_compat                                 // 6
   //    __HYPERVISOR_platform_op                                     // 7
   //    __HYPERVISOR_set_debugreg                                    // 8
   //    __HYPERVISOR_get_debugreg                                    // 9

   //    __HYPERVISOR_update_descriptor                               // 10
   //                                                                 // 11
   HYPXY(__HYPERVISOR_memory_op,               memory_op,         2), // 12
   //    __HYPERVISOR_multicall                                       // 13
   //    __HYPERVISOR_update_va_mapping                               // 14

   //    __HYPERVISOR_set_timer_op                                    // 15
   HYPXY(__HYPERVISOR_event_channel_op_compat, evtchn_op_compat,  1), // 16
   HYPXY(__HYPERVISOR_xen_version,             xen_version,       2), // 17
   //    __HYPERVISOR_console_io                                      // 18
   //    __HYPERVISOR_physdev_op_compat                               // 19

   HYPXY(__HYPERVISOR_grant_table_op,          grant_table_op,    3), // 20
   //    __HYPERVISOR_vm_assist                                       // 21
   //    __HYPERVISOR_update_va_mapping_otherdomain                   // 22
   //    __HYPERVISOR_iret,                    iret                   // 23
   //    __HYPERVISOR_vcpu_op,                 vcpu_op                // 24

   //    __HYPERVISOR_set_segment_base                                // 25
   HYPXY(__HYPERVISOR_mmuext_op,               mmuext_op,         2), // 26
   //    __HYPERVISOR_xsm_op                                          // 27
   //    __HYPERVISOR_nmi_op                                          // 28
   //    __HYPERVISOR_sched_op                                        // 29

   //    __HYPERVISOR_callback_op                                     // 30
   //    __HYPERVISOR_xenoprof_op                                     // 31
   HYPXY(__HYPERVISOR_event_channel_op,        evtchn_op,         2), // 32
   //    __HYPERVISOR_physdev_op                                      // 33
   HYPXY(__HYPERVISOR_hvm_op,                  hvm_op,            2), // 34

   HYPXY(__HYPERVISOR_sysctl,                  sysctl,            1), // 35
   HYPXY(__HYPERVISOR_domctl,                  domctl,            1), // 36
   //    __HYPERVISOR_kexec_op                                        // 37
   //    __HYPERVISOR_tmem_op                                         // 38
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
