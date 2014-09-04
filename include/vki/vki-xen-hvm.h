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
    vki_uint64_t first_pfn;
};
typedef struct vki_xen_hvm_set_mem_type vki_xen_hvm_set_mem_type_t;

#define VKI_XEN_HVMOP_set_mem_access        12
struct vki_xen_hvm_set_mem_access {
    vki_xen_domid_t domid;
    vki_uint16_t hvmmem_access;
    vki_uint32_t nr;
    vki_uint64_t first_pfn;
};
typedef struct vki_xen_hvm_set_mem_access vki_xen_hvm_set_mem_access_t;

#define VKI_XEN_HVMOP_get_mem_access        13
struct vki_xen_hvm_get_mem_access {
    vki_xen_domid_t domid;
    vki_uint16_t hvmmem_access; /* OUT */
    vki_uint64_t pfn;
};
typedef struct vki_xen_hvm_get_mem_access vki_xen_hvm_get_mem_access_t;

#define VKI_XEN_HVMOP_inject_trap            14
struct vki_xen_hvm_inject_trap {
    vki_xen_domid_t domid;
    vki_uint32_t vcpuid;
    vki_uint32_t vector;
    vki_uint32_t type;
    vki_uint32_t error_code;
    vki_uint32_t insn_len;
    vki_uint64_t cr2;
};
typedef struct vki_xen_hvm_inject_trap vki_xen_hvm_inject_trap_t;

#endif // __VKI_XEN_HVM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
