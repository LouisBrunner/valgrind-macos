#ifndef __VKI_XEN_DOMCTL_H
#define __VKI_XEN_DOMCTL_H

/*
 * The domctl interface is versioned via the interface_version
 * field. This structures in this header supports domctl interfaces:
 *
 * - 00000007: Xen 4.1
 * - 00000008: Xen 4.2
 * - 00000009: Xen 4.3
 *
 * When adding a new subop be sure to include the variants used by all
 * of the above, both here and in syswrap-xen.c
 *
 * Structs which are identical in all supported versions have no
 * version suffix. Structs which do differ are defined multiple times
 * and use the suffix of the latest version to contain that particular
 * variant.
 */

#define VKI_XEN_DOMCTL_createdomain                   1
#define VKI_XEN_DOMCTL_destroydomain                  2
#define VKI_XEN_DOMCTL_pausedomain                    3
#define VKI_XEN_DOMCTL_unpausedomain                  4
#define VKI_XEN_DOMCTL_getdomaininfo                  5
#define VKI_XEN_DOMCTL_getmemlist                     6
#define VKI_XEN_DOMCTL_getpageframeinfo               7
#define VKI_XEN_DOMCTL_getpageframeinfo2              8
#define VKI_XEN_DOMCTL_setvcpuaffinity                9
#define VKI_XEN_DOMCTL_shadow_op                     10
#define VKI_XEN_DOMCTL_max_mem                       11
#define VKI_XEN_DOMCTL_setvcpucontext                12
#define VKI_XEN_DOMCTL_getvcpucontext                13
#define VKI_XEN_DOMCTL_getvcpuinfo                   14
#define VKI_XEN_DOMCTL_max_vcpus                     15
#define VKI_XEN_DOMCTL_scheduler_op                  16
#define VKI_XEN_DOMCTL_setdomainhandle               17
#define VKI_XEN_DOMCTL_setdebugging                  18
#define VKI_XEN_DOMCTL_irq_permission                19
#define VKI_XEN_DOMCTL_iomem_permission              20
#define VKI_XEN_DOMCTL_ioport_permission             21
#define VKI_XEN_DOMCTL_hypercall_init                22
#define VKI_XEN_DOMCTL_arch_setup                    23
#define VKI_XEN_DOMCTL_settimeoffset                 24
#define VKI_XEN_DOMCTL_getvcpuaffinity               25
#define VKI_XEN_DOMCTL_real_mode_area                26
#define VKI_XEN_DOMCTL_resumedomain                  27
#define VKI_XEN_DOMCTL_sendtrigger                   28
#define VKI_XEN_DOMCTL_subscribe                     29
#define VKI_XEN_DOMCTL_gethvmcontext                 33
#define VKI_XEN_DOMCTL_sethvmcontext                 34
#define VKI_XEN_DOMCTL_set_address_size              35
#define VKI_XEN_DOMCTL_get_address_size              36
#define VKI_XEN_DOMCTL_assign_device                 37
#define VKI_XEN_DOMCTL_bind_pt_irq                   38
#define VKI_XEN_DOMCTL_memory_mapping                39
#define VKI_XEN_DOMCTL_ioport_mapping                40
#define VKI_XEN_DOMCTL_pin_mem_cacheattr             41
#define VKI_XEN_DOMCTL_set_ext_vcpucontext           42
#define VKI_XEN_DOMCTL_get_ext_vcpucontext           43
#define VKI_XEN_DOMCTL_set_opt_feature               44 /*Obsolete IA64 only */
#define VKI_XEN_DOMCTL_test_assign_device            45
#define VKI_XEN_DOMCTL_set_target                    46
#define VKI_XEN_DOMCTL_deassign_device               47
#define VKI_XEN_DOMCTL_unbind_pt_irq                 48
#define VKI_XEN_DOMCTL_set_cpuid                     49
#define VKI_XEN_DOMCTL_get_device_group              50
#define VKI_XEN_DOMCTL_set_machine_address_size      51
#define VKI_XEN_DOMCTL_get_machine_address_size      52
#define VKI_XEN_DOMCTL_suppress_spurious_page_faults 53
#define VKI_XEN_DOMCTL_debug_op                      54
#define VKI_XEN_DOMCTL_gethvmcontext_partial         55
#define VKI_XEN_DOMCTL_mem_event_op                  56
#define VKI_XEN_DOMCTL_mem_sharing_op                57
#define VKI_XEN_DOMCTL_disable_migrate               58
#define VKI_XEN_DOMCTL_gettscinfo                    59
#define VKI_XEN_DOMCTL_settscinfo                    60
#define VKI_XEN_DOMCTL_getpageframeinfo3             61
#define VKI_XEN_DOMCTL_setvcpuextstate               62
#define VKI_XEN_DOMCTL_getvcpuextstate               63
#define VKI_XEN_DOMCTL_set_access_required           64
#define VKI_XEN_DOMCTL_audit_p2m                     65
#define VKI_XEN_DOMCTL_set_virq_handler              66
#define VKI_XEN_DOMCTL_set_broken_page_p2m           67
#define VKI_XEN_DOMCTL_setnodeaffinity               68
#define VKI_XEN_DOMCTL_getnodeaffinity               69
#define VKI_XEN_DOMCTL_set_max_evtchn                70
#define VKI_XEN_DOMCTL_cacheflush                    71
#define VKI_XEN_DOMCTL_gdbsx_guestmemio            1000
#define VKI_XEN_DOMCTL_gdbsx_pausevcpu             1001
#define VKI_XEN_DOMCTL_gdbsx_unpausevcpu           1002
#define VKI_XEN_DOMCTL_gdbsx_domstatus             1003

struct vki_xen_domctl_createdomain {
    /* IN parameters */
    vki_uint32_t ssidref;
    vki_xen_domain_handle_t handle;
    vki_uint32_t flags;
};

struct vki_xen_domctl_getdomaininfo_00000007 {
    /* OUT variables. */
    vki_xen_domid_t  domain;
    vki_uint32_t flags;
    vki_xen_uint64_aligned_t tot_pages;
    vki_xen_uint64_aligned_t max_pages;
    vki_xen_uint64_aligned_t shr_pages;
    vki_xen_uint64_aligned_t paged_pages;
    vki_xen_uint64_aligned_t shared_info_frame;
    vki_xen_uint64_aligned_t cpu_time;
    vki_uint32_t nr_online_vcpus;
    vki_uint32_t max_vcpu_id;
    vki_uint32_t ssidref;
    vki_xen_domain_handle_t handle;
    vki_uint32_t cpupool;
};
typedef struct vki_xen_domctl_getdomaininfo_00000007 vki_xen_domctl_getdomaininfo_00000007_t;
DEFINE_VKI_XEN_GUEST_HANDLE(vki_xen_domctl_getdomaininfo_00000007_t);

struct vki_xen_domctl_getdomaininfo_00000008 {
    /* OUT variables. */
    vki_xen_domid_t  domain;
    vki_uint32_t flags;
    vki_xen_uint64_aligned_t tot_pages;
    vki_xen_uint64_aligned_t max_pages;
    vki_xen_uint64_aligned_t shr_pages;
    vki_xen_uint64_aligned_t paged_pages;
    vki_xen_uint64_aligned_t shared_info_frame;
    vki_xen_uint64_aligned_t cpu_time;
    vki_uint32_t nr_online_vcpus;
    vki_uint32_t max_vcpu_id;
    vki_uint32_t ssidref;
    vki_xen_domain_handle_t handle;
    vki_uint32_t cpupool;
};
typedef struct vki_xen_domctl_getdomaininfo_00000008 vki_xen_domctl_getdomaininfo_00000008_t;
DEFINE_VKI_XEN_GUEST_HANDLE(vki_xen_domctl_getdomaininfo_00000008_t);

struct vki_xen_domctl_getdomaininfo_00000009 {
    /* OUT variables. */
    vki_xen_domid_t  domain;
    vki_uint32_t flags;
    vki_xen_uint64_aligned_t tot_pages;
    vki_xen_uint64_aligned_t max_pages;
    vki_xen_uint64_aligned_t outstanding_pages;
    vki_xen_uint64_aligned_t shr_pages;
    vki_xen_uint64_aligned_t paged_pages;
    vki_xen_uint64_aligned_t shared_info_frame;
    vki_xen_uint64_aligned_t cpu_time;
    vki_uint32_t nr_online_vcpus;
    vki_uint32_t max_vcpu_id;
    vki_uint32_t ssidref;
    vki_xen_domain_handle_t handle;
    vki_uint32_t cpupool;
};
typedef struct vki_xen_domctl_getdomaininfo_00000009 vki_xen_domctl_getdomaininfo_00000009_t;
DEFINE_VKI_XEN_GUEST_HANDLE(vki_xen_domctl_getdomaininfo_00000009_t);

/* Get/set the NUMA node(s) with which the guest has affinity with. */
/* XEN_DOMCTL_setnodeaffinity */
/* XEN_DOMCTL_getnodeaffinity */
struct vki_xen_domctl_nodeaffinity {
    struct vki_xenctl_bitmap nodemap;/* IN */
};
typedef struct vki_xen_domctl_nodeaffinity vki_xen_domctl_nodeaffinity_t;
DEFINE_VKI_XEN_GUEST_HANDLE(vki_xen_domctl_nodeaffinity_t);

struct vki_xen_domctl_getpageframeinfo3 {
    vki_xen_uint64_aligned_t num; /* IN */
    VKI_XEN_GUEST_HANDLE_64(vki_xen_pfn_t) array; /* IN/OUT */
};

struct vki_xen_domctl_vcpuaffinity {
    vki_uint32_t  vcpu;              /* IN */
    struct vki_xenctl_bitmap cpumap; /* IN/OUT */
};

struct vki_xen_domctl_shadow_op_stats {
    vki_uint32_t fault_count;
    vki_uint32_t dirty_count;
};

/* vki_xen_domctl_shadow_op.op is an utter mess for compatibily reasons. */

struct vki_xen_domctl_shadow_op {
    vki_uint32_t op; /* IN */

#define VKI_XEN_DOMCTL_SHADOW_OP_OFF               0
#define VKI_XEN_DOMCTL_SHADOW_OP_ENABLE           32
#define VKI_XEN_DOMCTL_SHADOW_OP_CLEAN            11
#define VKI_XEN_DOMCTL_SHADOW_OP_PEEK             12
#define VKI_XEN_DOMCTL_SHADOW_OP_GET_ALLOCATION   30
#define VKI_XEN_DOMCTL_SHADOW_OP_SET_ALLOCATION   31

#define VKI_XEN_DOMCTL_SHADOW_OP_ENABLE_TEST       1
#define VKI_XEN_DOMCTL_SHADOW_OP_ENABLE_LOGDIRTY   2
#define VKI_XEN_DOMCTL_SHADOW_OP_ENABLE_TRANSLATE  3

    vki_uint32_t mode;

#define XEN_DOMCTL_SHADOW_ENABLE_REFCOUNT  (1 << 1)
#define XEN_DOMCTL_SHADOW_ENABLE_LOG_DIRTY (1 << 2)
#define XEN_DOMCTL_SHADOW_ENABLE_TRANSLATE (1 << 3)
#define XEN_DOMCTL_SHADOW_ENABLE_EXTERNAL  (1 << 4)

    vki_uint32_t mb;
    VKI_XEN_GUEST_HANDLE_64(vki_uint8) dirty_bitmap;
    vki_xen_uint64_aligned_t pages;
    struct vki_xen_domctl_shadow_op_stats stats;
};

struct vki_xen_domctl_max_mem {
    /* IN variables. */
    vki_xen_uint64_aligned_t max_memkb;
};

struct vki_xen_domctl_vcpucontext {
    vki_uint32_t              vcpu;                  /* IN */
    VKI_XEN_GUEST_HANDLE_64(vki_xen_vcpu_guest_context_t) ctxt; /* IN/OUT */
};

struct vki_xen_domctl_getvcpuinfo {
    /* IN variables. */
    vki_uint32_t vcpu;
    /* OUT variables. */
    vki_uint8_t  online;              /* currently online (not hotplugged)? */
    vki_uint8_t  blocked;             /* blocked waiting for an event? */
    vki_uint8_t  running;             /* currently scheduled on its CPU? */
    vki_xen_uint64_aligned_t cpu_time;/* total cpu time consumed (ns) */
    vki_uint32_t cpu;                 /* current mapping   */
};

struct vki_xen_domctl_scheduler_op {
    vki_uint32_t sched_id;  /* VKI_XEN_SCHEDULER_* */
#define VKI_XEN_SCHEDULER_SEDF     4
#define VKI_XEN_SCHEDULER_CREDIT   5
#define VKI_XEN_SCHEDULER_CREDIT2  6
#define VKI_XEN_SCHEDULER_ARINC653 7
    vki_uint32_t cmd;       /* VKI_XEN_DOMCTL_SCHEDOP_* */
#define VKI_XEN_DOMCTL_SCHEDOP_putinfo 0
#define VKI_XEN_DOMCTL_SCHEDOP_getinfo 1
    union {
        struct xen_domctl_sched_sedf {
            vki_xen_uint64_aligned_t period;
            vki_xen_uint64_aligned_t slice;
            vki_xen_uint64_aligned_t latency;
            vki_uint32_t extratime;
            vki_uint32_t weight;
        } sedf;
        struct xen_domctl_sched_credit {
            vki_uint16_t weight;
            vki_uint16_t cap;
        } credit;
        struct xen_domctl_sched_credit2 {
            vki_uint16_t weight;
        } credit2;
    } u;
};

struct vki_xen_domctl_max_vcpus {
    vki_uint32_t max;           /* maximum number of vcpus */
};

struct vki_xen_domctl_hypercall_init {
    vki_xen_uint64_aligned_t  gmfn;           /* GMFN to be initialised */
};

struct vki_xen_domctl_settimeoffset {
    vki_int32_t time_offset_seconds;
};

struct vki_xen_domctl_cpuid {
  vki_uint32_t input[2];
  vki_uint32_t eax;
  vki_uint32_t ebx;
  vki_uint32_t ecx;
  vki_uint32_t edx;
};

struct vki_xen_guest_tsc_info {
    vki_uint32_t tsc_mode;
    vki_uint32_t gtsc_khz;
    vki_uint32_t incarnation;
    vki_uint32_t pad;
    vki_xen_uint64_aligned_t elapsed_nsec;
};
typedef struct vki_xen_guest_tsc_info vki_xen_guest_tsc_info_t;
DEFINE_VKI_XEN_GUEST_HANDLE(vki_xen_guest_tsc_info_t);

struct vki_xen_domctl_hvmcontext {
    vki_uint32_t size; /* IN/OUT size of buffer */
    VKI_XEN_GUEST_HANDLE_64(vki_uint8) buffer; /* IN/OUT */
};
typedef struct vki_xen_domctl_hvmcontext vki_xen_domctl_hvmcontext_t;
DEFINE_VKI_XEN_GUEST_HANDLE(vki_xen_domctl_hvmcontext_t);

struct vki_xen_domctl_tsc_info {
    VKI_XEN_GUEST_HANDLE_64(vki_xen_guest_tsc_info_t) out_info; /* OUT */
    vki_xen_guest_tsc_info_t info; /* IN */
};

struct vki_xen_domctl_vcpuextstate {
    vki_uint32_t         vcpu;
    vki_xen_uint64_aligned_t         xfeature_mask;
    vki_xen_uint64_aligned_t         size;
    VKI_XEN_GUEST_HANDLE_64(vki_uint64) buffer;
};

struct vki_xen_domctl_address_size {
    vki_uint32_t size;
};

struct vki_xen_domctl_set_max_evtchn {
    vki_uint32_t max_port;
};

struct vki_xen_domctl_cacheflush {
    /* IN: page range to flush. */
    vki_xen_pfn_t start_pfn, nr_pfns;
};

struct vki_xen_domctl {
    vki_uint32_t cmd;
    vki_uint32_t interface_version; /* XEN_DOMCTL_INTERFACE_VERSION */
    vki_xen_domid_t  domain;
    union {
        struct vki_xen_domctl_createdomain      createdomain;
        struct vki_xen_domctl_getdomaininfo_00000007 getdomaininfo_00000007;
        struct vki_xen_domctl_getdomaininfo_00000008 getdomaininfo_00000008;
        struct vki_xen_domctl_getdomaininfo_00000009 getdomaininfo_00000009;
        //struct vki_xen_domctl_getmemlist        getmemlist;
        //struct vki_xen_domctl_getpageframeinfo  getpageframeinfo;
        //struct vki_xen_domctl_getpageframeinfo2 getpageframeinfo2;
        struct vki_xen_domctl_getpageframeinfo3 getpageframeinfo3;
        struct vki_xen_domctl_nodeaffinity      nodeaffinity;
        struct vki_xen_domctl_vcpuaffinity      vcpuaffinity;
        struct vki_xen_domctl_shadow_op         shadow_op;
        struct vki_xen_domctl_max_mem           max_mem;
        struct vki_xen_domctl_vcpucontext       vcpucontext;
        struct vki_xen_domctl_getvcpuinfo       getvcpuinfo;
        struct vki_xen_domctl_max_vcpus         max_vcpus;
        struct vki_xen_domctl_scheduler_op      scheduler_op;
        //struct vki_xen_domctl_setdomainhandle   setdomainhandle;
        //struct vki_xen_domctl_setdebugging      setdebugging;
        //struct vki_xen_domctl_irq_permission    irq_permission;
        //struct vki_xen_domctl_iomem_permission  iomem_permission;
        //struct vki_xen_domctl_ioport_permission ioport_permission;
        struct vki_xen_domctl_hypercall_init    hypercall_init;
        //struct vki_xen_domctl_arch_setup        arch_setup;
        struct vki_xen_domctl_settimeoffset     settimeoffset;
        //struct vki_xen_domctl_disable_migrate   disable_migrate;
        struct vki_xen_domctl_tsc_info          tsc_info;
        //struct vki_xen_domctl_real_mode_area    real_mode_area;
        struct vki_xen_domctl_hvmcontext        hvmcontext;
        //struct vki_xen_domctl_hvmcontext_partial hvmcontext_partial;
        struct vki_xen_domctl_address_size      address_size;
        //struct vki_xen_domctl_sendtrigger       sendtrigger;
        //struct vki_xen_domctl_get_device_group  get_device_group;
        //struct vki_xen_domctl_assign_device     assign_device;
        //struct vki_xen_domctl_bind_pt_irq       bind_pt_irq;
        //struct vki_xen_domctl_memory_mapping    memory_mapping;
        //struct vki_xen_domctl_ioport_mapping    ioport_mapping;
        //struct vki_xen_domctl_pin_mem_cacheattr pin_mem_cacheattr;
        //struct vki_xen_domctl_ext_vcpucontext   ext_vcpucontext;
        //struct vki_xen_domctl_set_target        set_target;
        //struct vki_xen_domctl_subscribe         subscribe;
        //struct vki_xen_domctl_debug_op          debug_op;
        //struct vki_xen_domctl_mem_event_op      mem_event_op;
        //struct vki_xen_domctl_mem_sharing_op    mem_sharing_op;
#if defined(__i386__) || defined(__x86_64__)
        struct vki_xen_domctl_cpuid             cpuid;
        struct vki_xen_domctl_vcpuextstate      vcpuextstate;
#endif
        //struct vki_xen_domctl_set_access_required access_required;
        //struct vki_xen_domctl_audit_p2m         audit_p2m;
        //struct vki_xen_domctl_set_virq_handler  set_virq_handler;
        struct vki_xen_domctl_set_max_evtchn    set_max_evtchn;
        //struct vki_xen_domctl_gdbsx_memio       gdbsx_guest_memio;
        //struct vki_xen_domctl_set_broken_page_p2m set_broken_page_p2m;
        struct vki_xen_domctl_cacheflush        cacheflush;
        //struct vki_xen_domctl_gdbsx_pauseunp_vcpu gdbsx_pauseunp_vcpu;
        //struct vki_xen_domctl_gdbsx_domstatus   gdbsx_domstatus;
        vki_uint8_t                         pad[128];
    } u;
};

#endif // __VKI_XEN_DOMCTL_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
