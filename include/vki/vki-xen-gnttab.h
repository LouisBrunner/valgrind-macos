#ifndef __VKI_XEN_GNTTAB_H
#define __VKI_XEN_GNTTAB_H

typedef vki_uint32_t vki_xen_grant_ref_t;

#define VKI_XEN_GNTTABOP_map_grant_ref        0
#define VKI_XEN_GNTTABOP_unmap_grant_ref      1
#define VKI_XEN_GNTTABOP_setup_table          2
#define VKI_XEN_GNTTABOP_dump_table           3
#define VKI_XEN_GNTTABOP_transfer             4
#define VKI_XEN_GNTTABOP_copy                 5
#define VKI_XEN_GNTTABOP_query_size           6
#define VKI_XEN_GNTTABOP_unmap_and_replace    7
#define VKI_XEN_GNTTABOP_set_version          8
#define VKI_XEN_GNTTABOP_get_status_frames    9
#define VKI_XEN_GNTTABOP_get_version          10
#define VKI_XEN_GNTTABOP_swap_grant_ref	      11

struct vki_xen_gnttab_setup_table {
    /* IN parameters. */
    vki_xen_domid_t  dom;
    vki_uint32_t nr_frames;
    /* OUT parameters. */
    vki_int16_t  status;              /* => enum grant_status */
    VKI_XEN_GUEST_HANDLE(vki_ulong) frame_list;
};

#endif // __VKI_XEN_GNTTAB_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
