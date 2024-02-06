/*
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 *
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * Carnegie Mellon requests users of this software to return to
 *
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 *
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
/*
 * HISTORY
 * $Log:mach_msg.c,v $
 * Revision 2.3  92/01/23  15:22:17  rpd
 * Fixed to not pass MACH_SEND_INTERRUPT and MACH_RCV_INTERRUPT
 * to the kernel.
 * [92/01/20            rpd]
 *
 * Revision 2.2  92/01/15  17:17:13  rpd
 * Created from msg.c.
 * [92/01/15            rpd]
 *
 */

#if defined(VGO_darwin)

#include "config.h" // for DARWIN_VERS
#include "pub_core_basics.h"
#include "pub_core_mach.h"

#include <mach/port.h>
#include <mach/message.h>

#if DARWIN_VERS >= DARWIN_13_00
// All of those should be defined in mach/message.h, but they are not.
typedef uint64_t mach_msg_option64_t;
#define MACH64_SEND_MSG MACH_SEND_MSG
#define MACH64_RCV_MSG MACH_RCV_MSG
#define MACH64_RCV_LARGE MACH_RCV_LARGE
#define MACH64_RCV_LARGE_IDENTITY MACH_RCV_LARGE_IDENTITY
#define MACH64_SEND_TIMEOUT MACH_SEND_TIMEOUT
#define MACH64_SEND_OVERRIDE MACH_SEND_OVERRIDE
#define MACH64_SEND_INTERRUPT MACH_SEND_INTERRUPT
#define MACH64_SEND_NOTIFY MACH_SEND_NOTIFY
#define MACH64_SEND_FILTER_NONFATAL MACH_SEND_FILTER_NONFATAL
#define MACH64_SEND_TRAILER MACH_SEND_TRAILER
#define MACH64_SEND_NOIMPORTANCE MACH_SEND_NOIMPORTANCE
#define MACH64_SEND_NODENAP MACH_SEND_NODENAP
#define MACH64_SEND_SYNC_OVERRIDE MACH_SEND_SYNC_OVERRIDE
#define MACH64_SEND_PROPAGATE_QOS MACH_SEND_PROPAGATE_QOS
#define MACH64_SEND_SYNC_BOOTSTRAP_CHECKIN MACH_SEND_SYNC_BOOTSTRAP_CHECKIN
#define MACH64_RCV_TIMEOUT MACH_RCV_TIMEOUT
#define MACH64_RCV_INTERRUPT MACH_RCV_INTERRUPT
#define MACH64_RCV_VOUCHER MACH_RCV_VOUCHER
#define MACH64_RCV_GUARDED_DESC MACH_RCV_GUARDED_DESC
#define MACH64_RCV_SYNC_WAIT MACH_RCV_SYNC_WAIT
#define MACH64_RCV_SYNC_PEEK MACH_RCV_SYNC_PEEK
#define MACH64_MSG_STRICT_REPLY MACH_MSG_STRICT_REPLY
#define MACH64_MSG_VECTOR 0x0000000100000000ull
#define MACH64_SEND_KOBJECT_CALL 0x0000000200000000ull
#define MACH64_SEND_MQ_CALL 0x0000000400000000ull
#define MACH64_SEND_ANY 0x0000000800000000ull

typedef struct {
	/* a mach_msg_header_t* or mach_msg_aux_header_t* */
	mach_vm_address_t               msgv_data;
	/* if msgv_rcv_addr is non-zero, use it as rcv address instead */
	mach_vm_address_t               msgv_rcv_addr;
	mach_msg_size_t                 msgv_send_size;
	mach_msg_size_t                 msgv_rcv_size;
} mach_msg_vector_t;

extern mach_msg_return_t
mach_msg2(void *data,
          mach_msg_option64_t options,
          uint64_t msgh_bits_and_send_size,
          uint64_t msgh_remote_and_local_port,
          uint64_t msgh_voucher_and_id,
          uint64_t desc_count_and_rcv_name,
          uint64_t rcv_size_and_priority,
          uint64_t timeout);
// end of defines

#define LIBMACH_OPTIONS64 (MACH64_SEND_INTERRUPT|MACH64_RCV_INTERRUPT)

extern mach_msg_return_t
mach_msg2_trap(void *data,
               mach_msg_option64_t options,
               uint64_t msgh_bits_and_send_size,
               uint64_t msgh_remote_and_local_port,
               uint64_t msgh_voucher_and_id,
               uint64_t desc_count_and_rcv_name,
               uint64_t rcv_size_and_priority,
               uint64_t timeout);

static inline mach_msg_option64_t
mach_msg_options_after_interruption(mach_msg_option64_t option64)
{
	if ((option64 & MACH64_SEND_MSG) && (option64 & MACH64_RCV_MSG)) {
		/*
		 * If MACH_RCV_SYNC_WAIT was passed for a combined send-receive it must
		 * be cleared for receive-only retries, as the kernel has no way to
		 * discover the destination.
		 */
		option64 &= ~MACH64_RCV_SYNC_WAIT;
	}
	option64 &= ~(LIBMACH_OPTIONS64 | MACH64_SEND_MSG);
	return option64;
}

mach_msg_return_t mach_msg2(
  void *data,
  mach_msg_option64_t option64,
  uint64_t msgh_bits_and_send_size,
  uint64_t msgh_remote_and_local_port,
  uint64_t msgh_voucher_and_id,
  uint64_t desc_count_and_rcv_name,
  uint64_t rcv_size_and_priority,
  uint64_t timeout
) {
  mach_msg_return_t mr;

	mr = mach_msg2_trap(data,
	    option64 & ~LIBMACH_OPTIONS64,
	    msgh_bits_and_send_size,
	    msgh_remote_and_local_port,
	    msgh_voucher_and_id,
	    desc_count_and_rcv_name,
	    rcv_size_and_priority,
	    timeout);

	if (mr == MACH_MSG_SUCCESS) {
		return MACH_MSG_SUCCESS;
	}

	if ((option64 & MACH64_SEND_INTERRUPT) == 0) {
		while (mr == MACH_SEND_INTERRUPTED) {
			mr = mach_msg2_trap(data,
			    option64 & ~LIBMACH_OPTIONS64,
			    msgh_bits_and_send_size,
			    msgh_remote_and_local_port,
			    msgh_voucher_and_id,
			    desc_count_and_rcv_name,
			    rcv_size_and_priority,
			    timeout);
		}
	}

	if ((option64 & MACH64_RCV_INTERRUPT) == 0) {
		while (mr == MACH_RCV_INTERRUPTED) {
			mr = mach_msg2_trap(data,
			    mach_msg_options_after_interruption(option64),
			    msgh_bits_and_send_size & 0xffffffffull, /* zero send size */
			    msgh_remote_and_local_port,
			    msgh_voucher_and_id,
			    desc_count_and_rcv_name,
			    rcv_size_and_priority,
			    timeout);
		}
	}

	return mr;
}
#endif


#define LIBMACH_OPTIONS (MACH_SEND_INTERRUPT|MACH_RCV_INTERRUPT)

extern mach_msg_return_t
mach_msg_trap(mach_msg_header_t *msg,
              mach_msg_option_t option,
              mach_msg_size_t send_size,
              mach_msg_size_t rcv_size,
              mach_port_t rcv_name,
              mach_msg_timeout_t timeout,
              mach_port_t notify);

mach_msg_return_t mach_msg(
  mach_msg_header_t *msg,
  mach_msg_option_t option,
  mach_msg_size_t send_size,
  mach_msg_size_t rcv_size,
  mach_port_t rcv_name,
  mach_msg_timeout_t timeout,
  mach_port_t notify
) {

#if DARWIN_VERS >= DARWIN_13_00
    mach_msg_base_t *base;
    if (option & MACH64_MSG_VECTOR) {
      base = (mach_msg_base_t *)((mach_msg_vector_t *)msg)->msgv_data;
    } else {
      base = (mach_msg_base_t *)msg;
        }

	  mach_msg_size_t descriptors = 0;
    if (option & MACH_SEND_MSG && msg->msgh_bits & MACH_MSGH_BITS_COMPLEX) {
      descriptors = base->body.msgh_descriptor_count;
    }

#define MACH_MSG2_SHIFT_ARGS(lo, hi) ((uint64_t)hi << 32 | (uint32_t)lo)
    return mach_msg2(
      msg,
      ((mach_msg_option64_t)option) | MACH64_SEND_KOBJECT_CALL,
      MACH_MSG2_SHIFT_ARGS(msg->msgh_bits, send_size),
      MACH_MSG2_SHIFT_ARGS(msg->msgh_remote_port, msg->msgh_local_port),
      MACH_MSG2_SHIFT_ARGS(msg->msgh_voucher_port, msg->msgh_id),
      MACH_MSG2_SHIFT_ARGS(descriptors, rcv_name),
      MACH_MSG2_SHIFT_ARGS(rcv_size, 0),
      timeout
    );
#undef MACH_MSG2_SHIFT_ARGS

#else
    mach_msg_return_t mr;

    /*
     * Consider the following cases:
     *1) Errors in pseudo-receive (eg, MACH_SEND_INTERRUPTED
     *plus special bits).
     *2) Use of MACH_SEND_INTERRUPT/MACH_RCV_INTERRUPT options.
     *3) RPC calls with interruptions in one/both halves.
     *
     * We refrain from passing the option bits that we implement
     * to the kernel.  This prevents their presence from inhibiting
     * the kernel's fast paths (when it checks the option value).
     */

    mr = mach_msg_trap(msg, option &~ LIBMACH_OPTIONS,
                       send_size, rcv_size, rcv_name,
                       timeout, notify);
    if (mr == MACH_MSG_SUCCESS)
        return MACH_MSG_SUCCESS;

    if ((option & MACH_SEND_INTERRUPT) == 0)
        while (mr == MACH_SEND_INTERRUPTED)
            mr = mach_msg_trap(msg,
                               option &~ LIBMACH_OPTIONS,
                               send_size, rcv_size, rcv_name,
                               timeout, notify);

    if ((option & MACH_RCV_INTERRUPT) == 0)
        while (mr == MACH_RCV_INTERRUPTED)
            mr = mach_msg_trap(msg,
                               option &~ (LIBMACH_OPTIONS|MACH_SEND_MSG),
                               0, rcv_size, rcv_name,
                               timeout, notify);

    return mr;
#endif
}

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
