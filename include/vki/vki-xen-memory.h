#ifndef __VKI_XEN_MEMORY_H
#define __VKI_XEN_MEMORY_H

#define VKI_XENMEM_increase_reservation 0
#define VKI_XENMEM_decrease_reservation 1
#define VKI_XENMEM_maximum_ram_page     2
#define VKI_XENMEM_current_reservation  3
#define VKI_XENMEM_maximum_reservation  4
#define VKI_XENMEM_machphys_mfn_list    5
#define VKI_XENMEM_populate_physmap     6
#define VKI_XENMEM_add_to_physmap       7
#define VKI_XENMEM_memory_map           9
#define VKI_XENMEM_machine_memory_map   10
#define VKI_XENMEM_exchange             11
#define VKI_XENMEM_machphys_mapping     12
#define VKI_XENMEM_set_memory_map       13
#define VKI_XENMEM_maximum_gpfn         14
#define VKI_XENMEM_remove_from_physmap  15
#define VKI_XENMEM_set_pod_target       16
#define VKI_XENMEM_get_pod_target       17
#define VKI_XENMEM_get_sharing_freed_pages    18
#define VKI_XENMEM_get_sharing_shared_pages   19
#define VKI_XENMEM_claim_pages                24

struct vki_xen_memory_map {
    unsigned int nr_entries;
    VKI_XEN_GUEST_HANDLE(void) buffer;
};

struct vki_xen_foreign_memory_map {
    vki_xen_domid_t domid;
    struct vki_xen_memory_map map;
};

struct xen_memory_reservation {
    VKI_XEN_GUEST_HANDLE(vki_xen_pfn_t) extent_start;
    vki_xen_ulong_t    nr_extents;
    unsigned int   extent_order;
    unsigned int   mem_flags;
    vki_xen_domid_t domid;
};

struct vki_xen_machphys_mfn_list {
    unsigned int max_extents; /* IN */
    VKI_XEN_GUEST_HANDLE(vki_xen_pfn_t) extent_start; /* OUT */
    unsigned int nr_extents; /* OUT */
};

struct vki_xen_add_to_physmap {
    vki_xen_domid_t domid;
    vki_uint16_t size;

#define VKI_XENMAPSPACE_shared_info  0
#define VKI_XENMAPSPACE_grant_table  1
#define VKI_XENMAPSPACE_gmfn         2
#define VKI_XENMAPSPACE_gmfn_range   3
#define VKI_XENMAPSPACE_gmfn_foreign 4

    unsigned int space;
    vki_xen_ulong_t idx;
    vki_xen_pfn_t gpfn;
};

struct vki_xen_remove_from_physmap {
    vki_xen_domid_t domid;
    vki_xen_pfn_t gpfn;
};

#endif // __VKI_XEN_MEMORY_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
