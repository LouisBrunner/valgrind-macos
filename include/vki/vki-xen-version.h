#ifndef __VKI_XEN_VERSION_H
#define __VKI_XEN_VERSION_H

#define VKI_XENVER_version      0
#define VKI_XENVER_extraversion 1
#define VKI_XENVER_compile_info 2
#define VKI_XENVER_capabilities 3
#define VKI_XENVER_changeset 4
#define VKI_XENVER_platform_parameters 5
#define VKI_XENVER_get_features 6
#define VKI_XENVER_pagesize 7
#define VKI_XENVER_guest_handle 8
#define VKI_XENVER_commandline 9

typedef char vki_xen_extraversion_t[16];

struct vki_xen_compile_info {
    char compiler[64];
    char compile_by[16];
    char compile_domain[32];
    char compile_date[32];
};

typedef char vki_xen_capabilities_info_t[1024];

typedef char vki_xen_changeset_info_t[64];

struct vki_xen_platform_parameters {
    unsigned long virt_start;
};

struct vki_xen_feature_info {
    unsigned int submap_idx;    /* IN: which 32-bit submap to return */
    vki_uint32_t     submap;        /* OUT: 32-bit submap */
};

typedef char vki_xen_commandline_t[1024];

#endif // __VKI_XEN_VERSION_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
