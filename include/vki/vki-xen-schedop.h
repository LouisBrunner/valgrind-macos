#ifndef __VKI_XEN_SCHED_OP_H
#define __VKI_XEN_SCHED_OP_H

#define VKI_XEN_SCHEDOP_yield           0

#define VKI_XEN_SCHEDOP_block           1

#define VKI_XEN_SCHEDOP_shutdown        2

#define VKI_XEN_SCHEDOP_poll            3

#define VKI_XEN_SCHEDOP_remote_shutdown 4
struct vki_xen_remote_shutdown {
    vki_xen_domid_t domain_id;
    unsigned int reason;
};
typedef struct vki_xen_remote_shutdown vki_xen_remote_shutdown_t;

#define VKI_XEN_SCHEDOP_shutdown_code   5

#define VKI_XEN_SCHEDOP_watchdog        6

#endif /* __VKI_XEN_SCHED_OP_H */
