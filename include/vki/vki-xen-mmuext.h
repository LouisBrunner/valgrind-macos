#ifndef __VKI_XEN_MMUEXT_H
#define __VKI_XEN_MMUEXT_H

#define VKI_XEN_MMUEXT_PIN_L1_TABLE      0
#define VKI_XEN_MMUEXT_PIN_L2_TABLE      1
#define VKI_XEN_MMUEXT_PIN_L3_TABLE      2
#define VKI_XEN_MMUEXT_PIN_L4_TABLE      3
#define VKI_XEN_MMUEXT_UNPIN_TABLE       4
#define VKI_XEN_MMUEXT_NEW_BASEPTR       5
#define VKI_XEN_MMUEXT_TLB_FLUSH_LOCAL   6
#define VKI_XEN_MMUEXT_INVLPG_LOCAL      7
#define VKI_XEN_MMUEXT_TLB_FLUSH_MULTI   8
#define VKI_XEN_MMUEXT_INVLPG_MULTI      9
#define VKI_XEN_MMUEXT_TLB_FLUSH_ALL    10
#define VKI_XEN_MMUEXT_INVLPG_ALL       11
#define VKI_XEN_MMUEXT_FLUSH_CACHE      12
#define VKI_XEN_MMUEXT_SET_LDT          13
#define VKI_XEN_MMUEXT_NEW_USER_BASEPTR 15
#define VKI_XEN_MMUEXT_CLEAR_PAGE       16
#define VKI_XEN_MMUEXT_COPY_PAGE        17
#define VKI_XEN_MMUEXT_FLUSH_CACHE_GLOBAL 18
#define VKI_XEN_MMUEXT_MARK_SUPER       19
#define VKI_XEN_MMUEXT_UNMARK_SUPER     20

struct vki_xen_mmuext_op {
    unsigned int cmd;
    union {
        /* [UN]PIN_TABLE, NEW_BASEPTR, NEW_USER_BASEPTR
         * CLEAR_PAGE, COPY_PAGE, [UN]MARK_SUPER */
        vki_xen_pfn_t     mfn;
        /* INVLPG_LOCAL, INVLPG_ALL, SET_LDT */
        unsigned long linear_addr;
    } arg1;
    union {
        /* SET_LDT */
        unsigned int nr_ents;
        /* TLB_FLUSH_MULTI, INVLPG_MULTI */
        VKI_XEN_GUEST_HANDLE(const_void) vcpumask;
        /* COPY_PAGE */
        vki_xen_pfn_t src_mfn;
    } arg2;
};

#endif // __VKI_XEN_MMUEXT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
