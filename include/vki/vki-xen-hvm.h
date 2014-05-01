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

#define VKI_XEN_HVMOP_set_isa_irq_level 3
struct vki_xen_hvm_set_isa_irq_level {
    vki_xen_domid_t  domid;
    vki_uint8_t  isa_irq;
    vki_uint8_t  level;
};
typedef struct vki_xen_hvm_set_isa_irq_level vki_xen_hvm_set_isa_irq_level_t;

#define VKI_XEN_HVMOP_set_pci_link_route 4
struct vki_xen_hvm_set_pci_link_route {
    vki_xen_domid_t  domid;
    vki_uint8_t  link;
    vki_uint8_t  isa_irq;
};
typedef struct vki_xen_hvm_set_pci_link_route vki_xen_hvm_set_pci_link_route_t;

#define VKI_XEN_HVMOP_set_mem_type 8
struct vki_xen_hvm_set_mem_type {
    vki_xen_domid_t  domid;
    vki_uint16_t hvmmem_type;
    vki_uint32_t nr;
    vki_xen_uint64_aligned_t first_pfn;
};
typedef struct vki_xen_hvm_set_mem_type vki_xen_hvm_set_mem_type_t;

#endif // __VKI_XEN_HVM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
