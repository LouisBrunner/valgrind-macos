#ifndef __VKI_XEN_HVM_H
#define __VKI_XEN_HVM_H

/* Get/set subcommands: extra argument == pointer to xen_hvm_param struct. */
#define VKI_XEN_HVMOP_set_param           0
#define VKI_XEN_HVMOP_get_param           1
struct vki_xen_hvm_param {
    vki_xen_domid_t  domid;    /* IN */
    vki_uint32_t index;    /* IN */
    vki_uint64_t value;    /* IN/OUT */
};

#endif // __VKI_XEN_HVM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
