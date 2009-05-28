
/*--------------------------------------------------------------------*/
/*--- Private syscalls header for Darwin.    priv_syswrap-darwin.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2009 Apple Inc.
      Greg Parker  gparker@apple.com

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PRIV_SYSWRAP_DARWIN_H
#define __PRIV_SYSWRAP_DARWIN_H

/* requires #include "priv_types_n_macros.h" */

// syswrap-darwin.c
Addr allocstack ( ThreadId tid );
void find_stack_segment ( ThreadId tid, Addr sp );
void start_thread_NORETURN ( Word arg );
void assign_port_name(mach_port_t port, const char *name);
void record_named_port(ThreadId tid, mach_port_t port, mach_port_right_t right, const char *name);

extern const SyscallTableEntry ML_(mach_trap_table)[];
extern const SyscallTableEntry ML_(syscall_table)[];
extern const SyscallTableEntry ML_(mdep_trap_table)[];

extern const UInt ML_(syscall_table_size);
extern const UInt ML_(mach_trap_table_size);
extern const UInt ML_(mdep_trap_table_size);

void VG_(show_open_ports)(void);

// Unix syscalls
DECL_TEMPLATE(darwin, sys_semget);
DECL_TEMPLATE(darwin, sys_semop);
DECL_TEMPLATE(darwin, sys_semctl);
DECL_TEMPLATE(darwin, sys_sem_open);
DECL_TEMPLATE(darwin, sys_sem_close);
DECL_TEMPLATE(darwin, sys_sem_unlink);
DECL_TEMPLATE(darwin, sys_sem_post);
DECL_TEMPLATE(darwin, sys_sem_init);
DECL_TEMPLATE(darwin, sys_sem_destroy);
DECL_TEMPLATE(darwin, sys_sem_wait_nocancel);
DECL_TEMPLATE(darwin, sys_sem_trywait);
DECL_TEMPLATE(darwin, sys_bsdthread_create);
DECL_TEMPLATE(darwin, sys_bsdthread_terminate);
DECL_TEMPLATE(darwin, sys_kqueue);
DECL_TEMPLATE(darwin, sys_kevent);
DECL_TEMPLATE(darwin, sys_bsdthread_register);
DECL_TEMPLATE(darwin, sys_workq_open);
DECL_TEMPLATE(darwin, sys_workq_ops);
DECL_TEMPLATE(darwin, sys___mac_syscall);
DECL_TEMPLATE(darwin, sys_exit);
DECL_TEMPLATE(darwin, sys_sigaction);
DECL_TEMPLATE(darwin, sys___pthread_canceled);
DECL_TEMPLATE(darwin, sys___pthread_markcancel);
DECL_TEMPLATE(darwin, sys___pthread_sigmask);
DECL_TEMPLATE(darwin, sys___disable_threadsignal);
DECL_TEMPLATE(darwin, sys_kdebug_trace);
DECL_TEMPLATE(darwin, sys_seteuid);
DECL_TEMPLATE(darwin, sys_setegid);
DECL_TEMPLATE(darwin, sys_listxattr);
DECL_TEMPLATE(darwin, sys_flistxattr);
DECL_TEMPLATE(darwin, sys_shmget);
DECL_TEMPLATE(darwin, sys_shm_open);
DECL_TEMPLATE(darwin, sys_statx);
DECL_TEMPLATE(darwin, sys_fchmod_extended);
DECL_TEMPLATE(darwin, sys_chmod_extended);
DECL_TEMPLATE(darwin, sys_accessx);
DECL_TEMPLATE(darwin, sys_chflags);
DECL_TEMPLATE(darwin, sys_fchflags);
DECL_TEMPLATE(darwin, sys_stat64);
DECL_TEMPLATE(darwin, sys_lstat64);
DECL_TEMPLATE(darwin, sys_fstat64);
DECL_TEMPLATE(darwin, sys_getfsstat);
DECL_TEMPLATE(darwin, sys_getattrlist);
DECL_TEMPLATE(darwin, sys_setattrlist);
DECL_TEMPLATE(darwin, sys_getdirentriesattr);
DECL_TEMPLATE(darwin, sys_fsctl);
DECL_TEMPLATE(darwin, sys_socket);
DECL_TEMPLATE(darwin, sys_setsockopt);
DECL_TEMPLATE(darwin, sys_getsockopt);
DECL_TEMPLATE(darwin, sys_connect);
DECL_TEMPLATE(darwin, sys_accept);
DECL_TEMPLATE(darwin, sys_sendto);
DECL_TEMPLATE(darwin, sys_recvfrom);
DECL_TEMPLATE(darwin, sys_sendmsg);
DECL_TEMPLATE(darwin, sys_recvmsg);
DECL_TEMPLATE(darwin, sys_shutdown);
DECL_TEMPLATE(darwin, sys_bind);
DECL_TEMPLATE(darwin, sys_listen);
DECL_TEMPLATE(darwin, sys_getsockname);
DECL_TEMPLATE(darwin, sys_getpeername);
DECL_TEMPLATE(darwin, sys_socketpair);
DECL_TEMPLATE(darwin, sys_gethostuuid);
DECL_TEMPLATE(darwin, sys_pipe);
DECL_TEMPLATE(darwin, sys_getlogin);
DECL_TEMPLATE(darwin, sys_ptrace);
DECL_TEMPLATE(darwin, sys_issetugid);
DECL_TEMPLATE(darwin, sys_getdtablesize);
DECL_TEMPLATE(darwin, sys_lseek);
DECL_TEMPLATE(darwin, sys_getdirentries);
DECL_TEMPLATE(darwin, sys_getdirentries64);
DECL_TEMPLATE(darwin, sys_statfs64);
DECL_TEMPLATE(darwin, sys_fstatfs64);
DECL_TEMPLATE(darwin, sys_csops);
DECL_TEMPLATE(darwin, sys_auditon);
DECL_TEMPLATE(darwin, sys_pathconf);
DECL_TEMPLATE(darwin, sys_fpathconf);
DECL_TEMPLATE(darwin, sys_shared_region_map_file_np);
DECL_TEMPLATE(darwin, sys_mmap);
DECL_TEMPLATE(darwin, sys_sysctl);
DECL_TEMPLATE(darwin, sys_sigpending);
DECL_TEMPLATE(darwin, sys_sigprocmask);
DECL_TEMPLATE(darwin, sys_sigsuspend);
DECL_TEMPLATE(darwin, sys_watchevent);
DECL_TEMPLATE(darwin, sys_waitevent);
DECL_TEMPLATE(darwin, sys_modwatch);
DECL_TEMPLATE(darwin, sys_getxattr);
DECL_TEMPLATE(darwin, sys_fgetxattr);
DECL_TEMPLATE(darwin, sys_setxattr);
DECL_TEMPLATE(darwin, sys_fsetxattr);
DECL_TEMPLATE(darwin, sys_initgroups);
DECL_TEMPLATE(darwin, sys_posix_spawn);
DECL_TEMPLATE(darwin, sys_settid);
DECL_TEMPLATE(darwin, sys_sendfile);
DECL_TEMPLATE(darwin, sys_fcntl);
DECL_TEMPLATE(darwin, sys_fcntl64);
DECL_TEMPLATE(darwin, sys_ioctl);
DECL_TEMPLATE(darwin, sys_futimes);
DECL_TEMPLATE(darwin, sys_FAKE_SIGRETURN);
DECL_TEMPLATE(darwin, sys_sigreturn);

// Mach message helpers
DECL_TEMPLATE(darwin, host_info);
DECL_TEMPLATE(darwin, host_page_size);
DECL_TEMPLATE(darwin, host_get_io_master);
DECL_TEMPLATE(darwin, host_get_clock_service);
DECL_TEMPLATE(darwin, host_request_notification);
DECL_TEMPLATE(darwin, mach_port_type);
DECL_TEMPLATE(darwin, mach_port_extract_member);
DECL_TEMPLATE(darwin, mach_port_allocate);
DECL_TEMPLATE(darwin, mach_port_deallocate);
DECL_TEMPLATE(darwin, mach_port_get_refs);
DECL_TEMPLATE(darwin, mach_port_mod_refs);
DECL_TEMPLATE(darwin, mach_port_get_set_status);
DECL_TEMPLATE(darwin, mach_port_destroy);
DECL_TEMPLATE(darwin, mach_port_request_notification);
DECL_TEMPLATE(darwin, mach_port_insert_right);
DECL_TEMPLATE(darwin, mach_port_get_attributes);
DECL_TEMPLATE(darwin, mach_port_set_attributes);
DECL_TEMPLATE(darwin, mach_port_insert_member);
DECL_TEMPLATE(darwin, task_get_special_port);
DECL_TEMPLATE(darwin, semaphore_create);
DECL_TEMPLATE(darwin, semaphore_destroy);
DECL_TEMPLATE(darwin, mach_ports_lookup);
DECL_TEMPLATE(darwin, task_threads);
DECL_TEMPLATE(darwin, task_suspend);
DECL_TEMPLATE(darwin, task_resume);
DECL_TEMPLATE(darwin, vm_allocate);
DECL_TEMPLATE(darwin, vm_deallocate);
DECL_TEMPLATE(darwin, vm_protect);
DECL_TEMPLATE(darwin, vm_inherit);
DECL_TEMPLATE(darwin, vm_read);
DECL_TEMPLATE(darwin, mach_vm_read);
DECL_TEMPLATE(darwin, vm_copy);
DECL_TEMPLATE(darwin, vm_read_overwrite);
DECL_TEMPLATE(darwin, vm_map);
DECL_TEMPLATE(darwin, vm_remap);
DECL_TEMPLATE(darwin, mach_make_memory_entry_64);
DECL_TEMPLATE(darwin, vm_purgable_control);
DECL_TEMPLATE(darwin, mach_vm_purgable_control);
DECL_TEMPLATE(darwin, mach_vm_allocate);
DECL_TEMPLATE(darwin, mach_vm_deallocate);
DECL_TEMPLATE(darwin, mach_vm_protect);
DECL_TEMPLATE(darwin, mach_vm_copy);
DECL_TEMPLATE(darwin, mach_vm_inherit);
DECL_TEMPLATE(darwin, mach_vm_map);
DECL_TEMPLATE(darwin, mach_vm_region_recurse);
DECL_TEMPLATE(darwin, thread_terminate);
DECL_TEMPLATE(darwin, thread_create);
DECL_TEMPLATE(darwin, thread_create_running);
DECL_TEMPLATE(darwin, thread_suspend);
DECL_TEMPLATE(darwin, thread_get_state);
DECL_TEMPLATE(darwin, thread_policy);
DECL_TEMPLATE(darwin, thread_info);
DECL_TEMPLATE(darwin, bootstrap_register);
DECL_TEMPLATE(darwin, bootstrap_look_up);
DECL_TEMPLATE(darwin, mach_msg_receive);
DECL_TEMPLATE(darwin, mach_msg_bootstrap);
DECL_TEMPLATE(darwin, mach_msg_host);
DECL_TEMPLATE(darwin, mach_msg_task);
DECL_TEMPLATE(darwin, mach_msg_thread);

// Mach traps
DECL_TEMPLATE(darwin, mach_msg_unhandled);
DECL_TEMPLATE(darwin, mach_msg);
DECL_TEMPLATE(darwin, mach_reply_port);
DECL_TEMPLATE(darwin, mach_thread_self);
DECL_TEMPLATE(darwin, mach_host_self);
DECL_TEMPLATE(darwin, mach_task_self);
DECL_TEMPLATE(darwin, syscall_thread_switch);
DECL_TEMPLATE(darwin, semaphore_signal);
DECL_TEMPLATE(darwin, semaphore_signal_all);
DECL_TEMPLATE(darwin, semaphore_signal_thread);
DECL_TEMPLATE(darwin, semaphore_wait);
DECL_TEMPLATE(darwin, semaphore_wait_signal);
DECL_TEMPLATE(darwin, semaphore_timedwait);
DECL_TEMPLATE(darwin, semaphore_timedwait_signal);
DECL_TEMPLATE(darwin, sys___semwait_signal);
DECL_TEMPLATE(darwin, task_for_pid);
DECL_TEMPLATE(darwin, pid_for_task);
DECL_TEMPLATE(darwin, mach_timebase_info);
DECL_TEMPLATE(darwin, mach_wait_until);
DECL_TEMPLATE(darwin, mk_timer_create);
DECL_TEMPLATE(darwin, mk_timer_destroy);
DECL_TEMPLATE(darwin, mk_timer_arm);
DECL_TEMPLATE(darwin, mk_timer_cancel);
DECL_TEMPLATE(darwin, iokit_user_client_trap);
DECL_TEMPLATE(darwin, swtch);
DECL_TEMPLATE(darwin, swtch_pri);

// Machine-dependent traps
DECL_TEMPLATE(darwin, pthread_set_self);

// syswrap-<arch>-darwin.c
#include <mach/mach.h>
extern 
void thread_state_from_vex(thread_state_t mach_generic, 
                           thread_state_flavor_t flavor, 
                           mach_msg_type_number_t count, 
                           VexGuestArchState *vex_generic);
extern
void thread_state_to_vex(const thread_state_t mach_generic, 
                         thread_state_flavor_t flavor, 
                         mach_msg_type_number_t count, 
                         VexGuestArchState *vex_generic);
extern 
ThreadState *build_thread(const thread_state_t state, 
                          thread_state_flavor_t flavor, 
                          mach_msg_type_number_t count);
extern
void hijack_thread_state(thread_state_t mach_generic, 
                         thread_state_flavor_t flavor, 
                         mach_msg_type_number_t count, 
                         ThreadState *tst);
extern
__attribute__((noreturn))
void call_on_new_stack_0_1 ( Addr stack,
			     Addr retaddr,
			     void (*f)(Word),
                             Word arg1 );

extern void pthread_hijack_asm(void);
extern void pthread_hijack(Addr self, Addr kport, Addr func, Addr func_arg, 
                           Addr stacksize, Addr flags, Addr sp);
extern void wqthread_hijack_asm(void);
extern void wqthread_hijack(Addr self, Addr kport, Addr stackaddr, Addr workitem, Int reuse, Addr sp);

extern Addr pthread_starter;
extern Addr wqthread_starter;
extern SizeT pthread_structsize;


#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
