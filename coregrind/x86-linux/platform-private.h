#ifndef _X86_LINUX_PRIVATE_H
#define _X86_LINUX_PRIVATE_H

/* These are addresses within VGA_(blocking_syscall).  See syscall.S for details. */
extern const Word VGA_(blksys_setup);
extern const Word VGA_(blksys_restart);
extern const Word VGA_(blksys_complete);
extern const Word VGA_(blksys_committed);

#endif /* _X86_LINUX_PRIVATE_H */
