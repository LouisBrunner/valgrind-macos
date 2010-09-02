
/*--------------------------------------------------------------------*/
/*--- Private syscalls header for Darwin.    priv_syswrap-darwin.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2010 Apple Inc.
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

void ML_(sync_mappings)(const HChar *when, const HChar *where, Int num);

// Unix syscalls.  
// GEN = it uses the generic wrapper
// NYI = wrapper not yet implemented in Valgrind
// NOC = the non-"_nocancel" wrapper is used
// old = the syscall no longer exists in Darwin
DECL_TEMPLATE(darwin, exit);                    // 1
// GEN fork 2
// GEN read 3
// GEN write 4
// GEN open 5
// GEN close 6
// GEN wait4 7
// old creat 8
// GEN link 9
// GEN unlink 10
// old execv 11
// GEN chdir 12
// GEN fchdir 13
// GEN mknod 14
// GEN chmod 15
// GEN chown 16
// old break 17
DECL_TEMPLATE(darwin, getfsstat);               // 18
// old lseek 19
// GEN getpid 20
// old mount 21
// old umount 22
// GEN setuid 23
// GEN getuid 24
// GEN geteuid 25
DECL_TEMPLATE(darwin, ptrace);                  // 26
DECL_TEMPLATE(darwin, recvmsg);                 // 27
DECL_TEMPLATE(darwin, sendmsg);                 // 28
DECL_TEMPLATE(darwin, recvfrom);                // 29
DECL_TEMPLATE(darwin, accept);                  // 30
DECL_TEMPLATE(darwin, getpeername);             // 31
DECL_TEMPLATE(darwin, getsockname);             // 32
// GEN access 33
DECL_TEMPLATE(darwin, chflags);                 // 34
DECL_TEMPLATE(darwin, fchflags);                // 35
// GEN sync 36
// GEN kill 37
// old stat 38
// GEN getppid 39
// old lstat 40
// GEN dup 41
DECL_TEMPLATE(darwin, pipe);                    // 42
// GEN getegid 43
// NYI profil
// old ktrace
DECL_TEMPLATE(darwin, sigaction);               // 46
// GEN getgid 47
DECL_TEMPLATE(darwin, sigprocmask);             // 48
DECL_TEMPLATE(darwin, getlogin);                // 49
// NYI setlogin 50
// NYI acct 51
DECL_TEMPLATE(darwin, sigpending);              // 52
// GEN sigaltstack 53
DECL_TEMPLATE(darwin, ioctl);                   // 54
// NYI reboot 55
// NYI revoke 56
// NYI symlink 57
// GEN readlink 58
// GEN execve 59
// GEN umask 60
// GEN chroot 61
// old fstat
// 63 used internally, reserved
// old getpagesize 64
// GEN msync 65
// GEN vfork 66
// old vread
// old vwrite
// old sbrk
// old sstk
// old mmap
// old vadvise
// GEN munmap 73
// GEN mprotect 74
// GEN madvise 75
// old vhangup
// old vlimit
// NYI mincore 78
// GEN getgroups 79
// NYI setgroups 80
// GEN getpgrp 81
// NYI setpgid 82
// GEN setitimer 83
// old wait
// NYI swapon 85
// GEN getitimer 86
// old gethostname
// old sethostname
DECL_TEMPLATE(darwin, getdtablesize);           // 89
// GEN dup2 90
// old getdopt
DECL_TEMPLATE(darwin, fcntl);                   // 92
// GEN select 93
// old setdopt
// GEN fsync 95
// GEN setpriority 96
DECL_TEMPLATE(darwin, socket);                  // 97
DECL_TEMPLATE(darwin, connect);                 // 98
// old accept
// GEN getpriority 100
// old send
// old recv
// old sigreturn
DECL_TEMPLATE(darwin, bind);                    // 104
DECL_TEMPLATE(darwin, setsockopt);              // 105
DECL_TEMPLATE(darwin, listen);                  // 106
// old vtimes
// old sigvec
// old sigblock
// old sigsetmask
DECL_TEMPLATE(darwin, sigsuspend);              // 111
// old sigstack
// old recvmsg
// old sendmsg
// old vtrace
// GEN gettimeofday 116
// GEN getrusage 117
DECL_TEMPLATE(darwin, getsockopt);              // 118
// old resuba
// GEN readv 120
// GEN writev 121
// NYI settimeofday 122
// GEN fchown 123
// GEN fchmod 124
// old recvfrom
// NYI setreuid 126
// NYI setregid 127
// GEN rename 128
// old truncate
// old ftruncate
// GEN flock 131
// NYI mkfifo 132
DECL_TEMPLATE(darwin, sendto);                  // 133
DECL_TEMPLATE(darwin, shutdown);                // 134
DECL_TEMPLATE(darwin, socketpair);              // 135
// GEN mkdir 136
// GEN rmdir 137
// GEN utimes 138
DECL_TEMPLATE(darwin, futimes);                 // 139
// NYI adjtime 140
// old getpeername
DECL_TEMPLATE(darwin, gethostuuid);             // 142
// old sethostid
// old getrlimit
// old setrlimit
// old killpg
// GEN setsid 147
// old setquota
// old qquota
// old getsockname
// NYI getpgid 151
// NYI setprivexec 152
// GEN pread 153
// GEN pwrite 154
// NYI nfssvc 155
// old getdirentries
// GEN statfs 157
// GEN fstatfs 158
// NYI unmount 159
// old async_daemon
// NYI getfh 161
// old getdomainname
// old setdomainname
// 164
// NYI quotactl 165
// old exportfs
DECL_TEMPLATE(darwin, mount);                   // 167
// old ustat
DECL_TEMPLATE(darwin, csops);                   // 169
// old table
// old wait3
// old rpause
// NYI waitid 173
// old getdents
// old gc_control
// NYI add_profil 176
// 177
// 178
// 179
DECL_TEMPLATE(darwin, kdebug_trace);            // 180
// GEN setgid 181
DECL_TEMPLATE(darwin, setegid);                 // 182
DECL_TEMPLATE(darwin, seteuid);                 // 183
DECL_TEMPLATE(darwin, sigreturn);               // 184
DECL_TEMPLATE(darwin, FAKE_SIGRETURN);
// NYI chud 185
// 186
// 187
// GEN stat 188
// GEN fstat 189
// GEN lstat 190
DECL_TEMPLATE(darwin, pathconf);            // 191
DECL_TEMPLATE(darwin, fpathconf);           // 192
// 193
// GEN getrlimit 194
// GEN setrlimit 195
DECL_TEMPLATE(darwin, getdirentries);       // 196
DECL_TEMPLATE(darwin, mmap);                // 197
// 198  __syscall
DECL_TEMPLATE(darwin, lseek);               // 199 (was UX64)
// GEN truncate 200
// GEN ftruncate 201
DECL_TEMPLATE(darwin, __sysctl);                // 202
// GEN mlock 203
// GEN munlock 204
// NYI undelete 205
// NYI ATsocket 206
// NYI ATgetmsg 207
// NYI ATputmsg 208
// NYI ATPsndreq 209
// NYI ATPsndrsp 210
// NYI ATPgetreq 211
// NYI ATPgetrsp 212
// 213  Reserved for AppleTalk
// NYI kqueue_from_portset_np 214
// NYI kqueue_portset_np 215
// NYI mkcomplex 216
// NYI statv 217
// NYI lstatv 218
// NYI fstatv 219
DECL_TEMPLATE(darwin, getattrlist);             // 220
DECL_TEMPLATE(darwin, setattrlist);             // 221
DECL_TEMPLATE(darwin, getdirentriesattr);       // 222
DECL_TEMPLATE(darwin, exchangedata);            // 223
// 224 checkuseraccess
// NYI searchfs 225
// GEN delete 226
// NYI copyfile 226
// 228
// 229
// GEN poll 230
DECL_TEMPLATE(darwin, watchevent);              // 231
DECL_TEMPLATE(darwin, waitevent);               // 232
DECL_TEMPLATE(darwin, modwatch);                // 233
DECL_TEMPLATE(darwin, getxattr);                // 234
DECL_TEMPLATE(darwin, fgetxattr);               // 235
DECL_TEMPLATE(darwin, setxattr);                // 236
DECL_TEMPLATE(darwin, fsetxattr);               // 237
DECL_TEMPLATE(darwin, removexattr);             // 238
DECL_TEMPLATE(darwin, fremovexattr);            // 239
DECL_TEMPLATE(darwin, listxattr);               // 240
DECL_TEMPLATE(darwin, flistxattr);              // 241
DECL_TEMPLATE(darwin, fsctl);                   // 242
DECL_TEMPLATE(darwin, initgroups);              // 243
DECL_TEMPLATE(darwin, posix_spawn);             // 244
// 245
// 246
// NYI nfsclnt 247
// NYI fhopen 248
// 249
// NYI minherit 250
// NYI semsys 251
// NYI msgsys 252
// NYI shmsys 253
DECL_TEMPLATE(darwin, semctl);                  // 254
DECL_TEMPLATE(darwin, semget);                  // 255
DECL_TEMPLATE(darwin, semop);                   // 256
// 257
// NYI msgctl 258
// NYI msgget 259
// NYI msgsnd 260
// NYI msgrcv 261
DECL_TEMPLATE(darwin, shmat);                   // 262
DECL_TEMPLATE(darwin, shmctl);                  // 263
DECL_TEMPLATE(darwin, shmdt);                   // 264
DECL_TEMPLATE(darwin, shmget);                  // 265
DECL_TEMPLATE(darwin, shm_open);                // 266
// NYI shm_unlink 267
DECL_TEMPLATE(darwin, sem_open);                // 268
DECL_TEMPLATE(darwin, sem_close);               // 269
DECL_TEMPLATE(darwin, sem_unlink);              // 270
DECL_TEMPLATE(darwin, sem_wait);                // 271
DECL_TEMPLATE(darwin, sem_trywait);             // 272
DECL_TEMPLATE(darwin, sem_post);                // 273
// NYI sem_getvalue 274
DECL_TEMPLATE(darwin, sem_init);                // 275
DECL_TEMPLATE(darwin, sem_destroy);             // 276
DECL_TEMPLATE(darwin, open_extended)            // 277
// NYI umask_extended 278
DECL_TEMPLATE(darwin, stat_extended);           // 279
DECL_TEMPLATE(darwin, lstat_extended);          // 280
DECL_TEMPLATE(darwin, fstat_extended);          // 281
DECL_TEMPLATE(darwin, chmod_extended);          // 282
DECL_TEMPLATE(darwin, fchmod_extended);         // 283
DECL_TEMPLATE(darwin, access_extended);         // 284
DECL_TEMPLATE(darwin, settid);                  // 285
// NYI gettid 286
// NYI setsgroups 287
// NYI getsgroups 288
// NYI setwgroups 289
// NYI getwgroups 290
// NYI mkfifo_extended 291
// NYI mkdir_extended 292
// NYI identitysvc 293
// NYI shared_region_check_np 294
// NYI shared_region_map_np 295
// old load_shared_file
// old reset_shared_file
// old new_system_shared_regions
// old shared_region_map_file_np
// old shared_region_make_private_np
// NYI __pthread_mutex_destroy 301
// NYI __pthread_mutex_init 302
// NYI __pthread_mutex_lock 303
// NYI __pthread_mutex_trylock 304
// NYI __pthread_mutex_unlock 305
// NYI __pthread_cond_init 306
// NYI __pthread_cond_destroy 307
// NYI __pthread_cond_broadcast 308
// NYI __pthread_cond_signal 309
// NYI getsid 310
// NYI settid_with_pid 311
// NYI __pthread_cond_timedwait 312
// NYI aio_fsync 313
DECL_TEMPLATE(darwin, aio_return);             // 314
DECL_TEMPLATE(darwin, aio_suspend);            // 315
// NYI aio_cancel 316
DECL_TEMPLATE(darwin, aio_error);              // 317
DECL_TEMPLATE(darwin, aio_read);               // 318
DECL_TEMPLATE(darwin, aio_write);              // 319
// NYI lio_listio 320
// NYI __pthread_cond_wait 321
// NYI iopolicysys 322
// 323
// NYI mlockall 324
// NYI munlockall 325
// 326
DECL_TEMPLATE(darwin, issetugid);               // 327
DECL_TEMPLATE(darwin, __pthread_kill);          // 328
DECL_TEMPLATE(darwin, __pthread_sigmask);       // 329
// NYI __sigwait 330
DECL_TEMPLATE(darwin, __disable_threadsignal);  // 331
DECL_TEMPLATE(darwin, __pthread_markcancel);    // 332
DECL_TEMPLATE(darwin, __pthread_canceled);      // 333
DECL_TEMPLATE(darwin, __semwait_signal);        // 334
// old utrace
#if DARWIN_VERS >= DARWIN_10_6
DECL_TEMPLATE(darwin, proc_info);               // 336
#endif
DECL_TEMPLATE(darwin, sendfile);                // 337
DECL_TEMPLATE(darwin, stat64);                  // 338
DECL_TEMPLATE(darwin, fstat64);                 // 339
DECL_TEMPLATE(darwin, lstat64);                 // 340
DECL_TEMPLATE(darwin, stat64_extended);         // 341
DECL_TEMPLATE(darwin, lstat64_extended);        // 342
DECL_TEMPLATE(darwin, fstat64_extended);        // 343
DECL_TEMPLATE(darwin, getdirentries64);         // 344
DECL_TEMPLATE(darwin, statfs64);                // 345
DECL_TEMPLATE(darwin, fstatfs64);               // 346
DECL_TEMPLATE(darwin, getfsstat64);             // 347
// NYI __pthread_chdir 348
// NYI __pthread_fchdir 349
// NYI audit 350
DECL_TEMPLATE(darwin, auditon);                 // 351
// 352
// NYI getauid 353
// NYI setauid 354
// NYI getaudit 355
// NYI setaudit 356
// NYI getaudit_addr 357
// NYI setaudit_addr 358
// NYI auditctl 359
DECL_TEMPLATE(darwin, bsdthread_create);        // 360
DECL_TEMPLATE(darwin, bsdthread_terminate);     // 361
DECL_TEMPLATE(darwin, kqueue);                  // 362
DECL_TEMPLATE(darwin, kevent);                  // 363
// NYI lchown 364
// NYI stack_snapshot 365
DECL_TEMPLATE(darwin, bsdthread_register);      // 366
DECL_TEMPLATE(darwin, workq_open);              // 367
DECL_TEMPLATE(darwin, workq_ops);               // 368
// 369
// 370
// 371
DECL_TEMPLATE(darwin, __thread_selfid);         // 372
// 373
// 374
// 375
// 376
// 377
// 378
// 379
// NYI __mac_execve 380
DECL_TEMPLATE(darwin, __mac_syscall);           // 381
// NYI __mac_get_file 382
// NYI __mac_set_file 383
// NYI __mac_get_link 384
// NYI __mac_set_link 385
// NYI __mac_get_proc 386
// NYI __mac_set_proc 387
// NYI __mac_get_fd 388
// NYI __mac_set_fd 389
// NYI __mac_get_pid 390
// NYI __mac_get_lcid 391
// NYI __mac_get_lctx 392
// NYI __mac_set_lctx 393
// NYI setlcid 394
// NYI getlcid 395
// GEN read_nocancel 396
// GEN write_nocancel 397
// GEN open_nocancel 398
// GEN close_nocancel 399
// GEN wait4_nocancel 400
// NOC recvmsg_nocancel 401
// NOC sendmsg_nocancel 402
// NOC recvfrom_nocancel 403
// NOC accept_nocancel 404
// GEN msync_nocancel 405
// NOC fcntl_nocancel 406
// GEN select_nocancel 407
// GEN fsync_nocancel 408
// NOC connect_nocancel 409
// NYI sigsuspend_nocancel 410
// GEN readv_nocancel 411
// GEN writev_nocancel 412
// NOC sendto_nocancel 413
// GEN pread_nocancel 414
// GEN pwrite_nocancel 415
// NYI waitid_nocancel 416
// GEN poll_nocancel 417
// NYI msgsnd_nocancel 418
// NYI msgrcv_nocancel 419
// NOC sem_wait_nocancel 420
// NYI aio_suspend_nocancel 421
// NYI __sigwait_nocancel 422
// NOC __semwait_signal_nocancel 423
// NYI __mac_mount 424
// NYI __mac_get_mount 425
// NYI __mac_getfsstat 426
DECL_TEMPLATE(darwin, fsgetpath);                // 427
DECL_TEMPLATE(darwin, audit_session_self);       // 428
// NYI audit_session_join 429

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
DECL_TEMPLATE(darwin, mach_port_move_member);
DECL_TEMPLATE(darwin, mach_port_destroy);
DECL_TEMPLATE(darwin, mach_port_request_notification);
DECL_TEMPLATE(darwin, mach_port_insert_right);
DECL_TEMPLATE(darwin, mach_port_extract_right);
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
DECL_TEMPLATE(darwin, thread_policy_set);
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
DECL_TEMPLATE(darwin, thread_fast_set_cthread_self);

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
