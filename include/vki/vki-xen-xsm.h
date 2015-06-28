#ifndef __VKI_XEN_XSM_H
#define __VKI_XEN_XSM_H

#define VKI_XEN_FLASK_INTERFACE_VERSION 1

struct vki_xen_flask_sid_context {
    /* IN/OUT: sid to convert to/from string */
    vki_uint32_t sid;
    /* IN: size of the context buffer
     * OUT: actual size of the output context string
     */
    vki_uint32_t size;
    VKI_XEN_GUEST_HANDLE(char) context;
};

struct vki_xen_flask_op {
    vki_uint32_t cmd;
#define VKI_FLASK_LOAD              1
#define VKI_FLASK_GETENFORCE        2
#define VKI_FLASK_SETENFORCE        3
#define VKI_FLASK_CONTEXT_TO_SID    4
#define VKI_FLASK_SID_TO_CONTEXT    5
#define VKI_FLASK_ACCESS            6
#define VKI_FLASK_CREATE            7
#define VKI_FLASK_RELABEL           8
#define VKI_FLASK_USER              9
#define VKI_FLASK_POLICYVERS        10
#define VKI_FLASK_GETBOOL           11
#define VKI_FLASK_SETBOOL           12
#define VKI_FLASK_COMMITBOOLS       13
#define VKI_FLASK_MLS               14
#define VKI_FLASK_DISABLE           15
#define VKI_FLASK_GETAVC_THRESHOLD  16
#define VKI_FLASK_SETAVC_THRESHOLD  17
#define VKI_FLASK_AVC_HASHSTATS     18
#define VKI_FLASK_AVC_CACHESTATS    19
#define VKI_FLASK_MEMBER            20
#define VKI_FLASK_ADD_OCONTEXT      21
#define VKI_FLASK_DEL_OCONTEXT      22
#define VKI_FLASK_GET_PEER_SID      23
#define VKI_FLASK_RELABEL_DOMAIN    24
    vki_uint32_t interface_version; /* VKI_XEN_FLASK_INTERFACE_VERSION */
    union {
        //struct vki_xen_flask_load load;
        //struct vki_xen_flask_setenforce enforce;
        /* FLASK_CONTEXT_TO_SID and FLASK_SID_TO_CONTEXT */
        struct vki_xen_flask_sid_context sid_context;
        //struct vki_xen_flask_access access;
        /* FLASK_CREATE, FLASK_RELABEL, FLASK_MEMBER */
        //struct vki_xen_flask_transition transition;
        //struct vki_xen_flask_userlist userlist;
        /* FLASK_GETBOOL, FLASK_SETBOOL */
        //struct vki_xen_flask_boolean boolean;
        //struct vki_xen_flask_setavc_threshold setavc_threshold;
        //struct vki_xen_flask_hash_stats hash_stats;
        //struct vki_xen_flask_cache_stats cache_stats;
        /* FLASK_ADD_OCONTEXT, FLASK_DEL_OCONTEXT */
        //struct vki_xen_flask_ocontext ocontext;
        //struct vki_xen_flask_peersid peersid;
        //struct vki_xen_flask_relabel relabel;
    } u;
};

#endif // __VKI_XEN_XSM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
