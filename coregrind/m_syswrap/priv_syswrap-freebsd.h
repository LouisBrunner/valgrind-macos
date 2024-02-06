
/*--------------------------------------------------------------------*/
/*--- FreeBSD-specific syscalls stuff.          priv_syswrap-freebsd.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2008 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2018-2021 Paul Floyd
      pjfloyd@wanadoo.fr

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef PRIV_SYSWRAP_FREEBSD_H
#define PRIV_SYSWRAP_FREEBSD_H

/* requires #include "priv_types_n_macros.h" */
#include "priv_types_n_macros.h"
#include "config.h"

// Clone-related functions
extern Word ML_(start_thread_NORETURN) ( void* arg );
extern Addr ML_(allocstack)            ( ThreadId tid );
extern void ML_(call_on_new_stack_0_1) ( Addr stack, Addr retaddr,
                                         void (*f)(Word), Word arg1 );
extern SysRes ML_(do_fork) ( ThreadId tid );
extern SysRes ML_(do_vfork) ( ThreadId tid );
extern SysRes ML_(do_rfork) ( ThreadId tid, Int flags );


DECL_TEMPLATE(freebsd, sys_syscall)
DECL_TEMPLATE(freebsd, sys_exit) // 1
DECL_TEMPLATE(freebsd, sys_fork) // 2
// generic read 3
// generic write 4
// generic open 5
// generic close 6
// generic wait4 7
// generic link 9
// generic unlink 10
// generic chdir 12
// generic fchdir 13
// generic mknod 14
// generic chmod 15
// generic chown 16
// generic brk 17
DECL_TEMPLATE(freebsd, sys_mount) // 21
DECL_TEMPLATE(freebsd, sys_unmount) // 22
DECL_TEMPLATE(freebsd, sys_ptrace) // 26
DECL_TEMPLATE(freebsd, sys_recvmsg) // 27
DECL_TEMPLATE(freebsd, sys_sendmsg) // 28
DECL_TEMPLATE(freebsd, sys_recvfrom) // 29
DECL_TEMPLATE(freebsd, sys_accept) // 30
DECL_TEMPLATE(freebsd, sys_getpeername) // 31
DECL_TEMPLATE(freebsd, sys_getsockname) // 32
// generic access 33
DECL_TEMPLATE(freebsd, sys_chflags) // 34
DECL_TEMPLATE(freebsd, sys_fchflags) // 35
// generic sync 36
// generic kill 37
// generic getppid 39
// generic dup 41
DECL_TEMPLATE(freebsd, sys_pipe) // 42
// generic getegid 43
// generic profil redirect to ni_syscall 44
// sys_ktrace refirect to ni_syscall 45
// generic getgid 47
DECL_TEMPLATE(freebsd, sys_getlogin) // 49
DECL_TEMPLATE(freebsd, sys_setlogin) // 50
// generic acct 51
// generic sigaltstack 53
DECL_TEMPLATE(freebsd, sys_ioctl) // 54
DECL_TEMPLATE(freebsd, sys_reboot) // 55
DECL_TEMPLATE(freebsd, sys_revoke) // 56
// generic symlink 57
DECL_TEMPLATE(freebsd, sys_readlink) // 58
// generic execve 59
// generic umask 60
// generic chroot 61
// generic msync 65
DECL_TEMPLATE(freebsd, sys_vfork) // 66
DECL_TEMPLATE(freebsd, sys_sbrk) // 69

// freebsd11 vadvise 72
// generic munmap 73
// generic mprotect 74
// generic madvise 75
// generic mincore 78
// generic getgroups 79
// generic setgroups 80
// generic getpgrp 81
// generic setpgid 82
// generic setitimer 83

DECL_TEMPLATE(freebsd, sys_swapon) // 85
// generic getitimer 86
DECL_TEMPLATE(freebsd, sys_getdtablesize) // 90
DECL_TEMPLATE(freebsd, sys_fcntl) // 92
// generic select 93
// generic fsync 95
// generic setpriority 96
DECL_TEMPLATE(freebsd, sys_socket) // 97
DECL_TEMPLATE(freebsd, sys_connect) // 98
// generic getpriority 100
DECL_TEMPLATE(freebsd, sys_bind) // 104
DECL_TEMPLATE(freebsd, sys_setsockopt) // 105
DECL_TEMPLATE(freebsd, sys_listen) // 106
// generic gettimeofday 116
// generic rusage 117
DECL_TEMPLATE(freebsd, sys_getsockopt) // 118

// generic readv 120
// generic writev 121
// generic settimeofday 122
// generic fchown 123
// generic fchmod 124
// generic setreuid 126
// generic setregid 127
// generic rename 128
// generic flock 131
DECL_TEMPLATE(freebsd, sys_mkfifo) // 132
DECL_TEMPLATE(freebsd, sys_sendto) // 133
DECL_TEMPLATE(freebsd, sys_shutdown) // 134
DECL_TEMPLATE(freebsd, sys_socketpair) // 135
// generic mkdir 136
// generic rmdir 137
// generic utims 138
DECL_TEMPLATE(freebsd, sys_adjtime) // 140
// generic setsid 147
DECL_TEMPLATE(freebsd, sys_quotactl) // 148
// unimp nlm_syscall 154
//DECL_TEMPLATE(freebsd, sys_nfssvc) 155
DECL_TEMPLATE(freebsd, sys_lgetfh) // 160
DECL_TEMPLATE(freebsd, sys_getfh) // 161
#if (FREEBSD_VERS <= FREEBSD_10)
DECL_TEMPLATE(freebsd, sys_freebsd4_getdomainname) // 162
DECL_TEMPLATE(freebsd, sys_freebsd4_setdomainname) // 163
DECL_TEMPLATE(freebsd, sys_freebsd4_uname) // 164
#endif
DECL_TEMPLATE(freebsd, sys_sysarch) // 165
DECL_TEMPLATE(freebsd, sys_rtprio) // 166
//DECL_TEMPLATE(freebsd, sys_semsys) 169
//DECL_TEMPLATE(freebsd, sys_msgsys) 170
//DECL_TEMPLATE(freebsd, sys_shmsys) 171
#if (FREEBSD_VERS <= FREEBSD_10)
DECL_TEMPLATE(freebsd, sys_freebsd6_pread) // 173
DECL_TEMPLATE(freebsd, sys_freebsd6_pwrite) // 174
#endif
DECL_TEMPLATE(freebsd, sys_setfib) // 175
//DECL_TEMPLATE(freebsd, sys_ntp_adjtime) 176
// generic setgid 181
DECL_TEMPLATE(freebsd, sys_setegid) // 182
DECL_TEMPLATE(freebsd, sys_seteuid) // 183
#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_stat) // 188
DECL_TEMPLATE(freebsd, sys_freebsd11_fstat) // 189
DECL_TEMPLATE(freebsd, sys_freebsd11_lstat)// 190
#else
DECL_TEMPLATE(freebsd, sys_stat) // 188
DECL_TEMPLATE(freebsd, sys_fstat) // 189
DECL_TEMPLATE(freebsd, sys_lstat) // 190
#endif
DECL_TEMPLATE(freebsd, sys_pathconf) // 191
DECL_TEMPLATE(freebsd, sys_fpathconf) // 192
// generic getrlimit 194
// generic setrlimit 195
#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_getdirentries) // 196
#else
DECL_TEMPLATE(freebsd, sys_getdirentries) // 196
#endif

#if (FREEBSD_VERS <= FREEBSD_10)
DECL_TEMPLATE(freebsd, sys_freebsd6_mmap) // 197
#endif

#if (FREEBSD_VERS <= FREEBSD_10)
DECL_TEMPLATE(freebsd, sys_freebsd6_lseek) // 199
DECL_TEMPLATE(freebsd, sys_freebsd6_truncate) // 200
DECL_TEMPLATE(freebsd, sys_freebsd6_ftruncate) // 201
#endif
DECL_TEMPLATE(freebsd, sys___sysctl) // 202
// generic mlock 202
// generic munlock 203
DECL_TEMPLATE(freebsd, sys_undelete) // 205
DECL_TEMPLATE(freebsd, sys_futimes) // 206
// generic getpgod 207
// generic poll 209
DECL_TEMPLATE(freebsd, sys_freebsd7___semctl) // 220
DECL_TEMPLATE(freebsd, sys_semget) // 221
DECL_TEMPLATE(freebsd, sys_semop) // 222
DECL_TEMPLATE(freebsd, sys_freebsd7_msgctl) // 224
DECL_TEMPLATE(freebsd, sys_msgget) // 225
DECL_TEMPLATE(freebsd, sys_msgsnd) // 226
DECL_TEMPLATE(freebsd, sys_msgrcv) // 227
DECL_TEMPLATE(freebsd, sys_shmat) // 228
DECL_TEMPLATE(freebsd, sys_freebsd7_shmctl) // 229
DECL_TEMPLATE(freebsd, sys_shmdt) // 230
DECL_TEMPLATE(freebsd, sys_shmget) // 231
DECL_TEMPLATE(freebsd, sys_clock_gettime) // 232
DECL_TEMPLATE(freebsd, sys_clock_settime) // 233
DECL_TEMPLATE(freebsd, sys_clock_getres) // 234
DECL_TEMPLATE(freebsd, sys_timer_create) // 235
DECL_TEMPLATE(freebsd, sys_timer_delete) // 236
DECL_TEMPLATE(freebsd, sys_timer_settime) // 237
DECL_TEMPLATE(freebsd, sys_timer_gettime) // 238
DECL_TEMPLATE(freebsd, sys_timer_getoverrun) // 239
// generic sys_nanosleep 240
// unimpl ffclock_getcounter 241
// unimpl ffclock_setestimate 242
// unimpl ffclock_getestimate 243
DECL_TEMPLATE(freebsd, sys_clock_nanosleep) // 244
DECL_TEMPLATE(freebsd, sys_clock_getcpuclockid2) // 247
// unimpl ntp_gettime 248
DECL_TEMPLATE(freebsd, sys_minherit) // 250
DECL_TEMPLATE(freebsd, sys_rfork) // 251
DECL_TEMPLATE(freebsd, sys_issetugid) // 253
// generic lchown 254
DECL_TEMPLATE(freebsd, sys_aio_read) // 255
DECL_TEMPLATE(freebsd, sys_aio_write) // 256
DECL_TEMPLATE(freebsd, sys_lio_listio) // 257
// generic sys_getdents 272
DECL_TEMPLATE(freebsd, sys_lchmod) // 274
DECL_TEMPLATE(freebsd, sys_lutimes) // 276

// unimpl freebsd11_nstat 278
// unimpl freebsd11_nfstat 279
// unimpl freebsd11_nlstat 280
DECL_TEMPLATE(freebsd, sys_preadv) // 289
DECL_TEMPLATE(freebsd, sys_pwritev) // 290

DECL_TEMPLATE(freebsd, sys_fhopen) // 298

#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_fhstat) // 299
#else
DECL_TEMPLATE(freebsd, sys_fhstat) // 299
#endif

DECL_TEMPLATE(freebsd, sys_modnext) // 300
DECL_TEMPLATE(freebsd, sys_modstat) // 301
DECL_TEMPLATE(freebsd, sys_modfnext) // 302
DECL_TEMPLATE(freebsd, sys_modfind) // 303

DECL_TEMPLATE(freebsd, sys_kldload) // 304
DECL_TEMPLATE(freebsd, sys_kldunload) // 305
DECL_TEMPLATE(freebsd, sys_kldfind) // 306
DECL_TEMPLATE(freebsd, sys_kldnext) // 307
DECL_TEMPLATE(freebsd, sys_kldstat) // 308
DECL_TEMPLATE(freebsd, sys_kldfirstmod) // 309
DECL_TEMPLATE(freebsd, sys_setresuid) // 311
DECL_TEMPLATE(freebsd, sys_setresgid) // 312
DECL_TEMPLATE(freebsd, sys_aio_return) //314
DECL_TEMPLATE(freebsd, sys_aio_suspend) // 315
DECL_TEMPLATE(freebsd, sys_aio_cancel) // 316
DECL_TEMPLATE(freebsd, sys_aio_error) // 317
DECL_TEMPLATE(freebsd, sys_yield) // 321
DECL_TEMPLATE(freebsd, sys_munlockall) // 325
DECL_TEMPLATE(freebsd, sys___getcwd) // 326
DECL_TEMPLATE(freebsd, sys_sched_setparam) // 327
DECL_TEMPLATE(freebsd, sys_sched_getparam) // 328
DECL_TEMPLATE(freebsd, sys_sched_setscheduler) // 329
DECL_TEMPLATE(freebsd, sys_sched_getscheduler) // 330
DECL_TEMPLATE(freebsd, sys_sched_yield) // 331
DECL_TEMPLATE(freebsd, sys_sched_get_priority_max) // 332
DECL_TEMPLATE(freebsd, sys_sched_get_priority_min) // 333
DECL_TEMPLATE(freebsd, sys_sched_rr_get_interval) // 334
DECL_TEMPLATE(freebsd, sys_utrace) // 335
DECL_TEMPLATE(freebsd, sys_kldsym) // 337
DECL_TEMPLATE(freebsd, sys_jail) // 338
// unimpl SYS_nnpfs_syscall 339
DECL_TEMPLATE(freebsd, sys_sigprocmask) // 340
DECL_TEMPLATE(freebsd, sys_sigsuspend) // 341

DECL_TEMPLATE(freebsd, sys_sigpending) // 343

DECL_TEMPLATE(freebsd, sys_sigtimedwait) // 345
DECL_TEMPLATE(freebsd, sys_sigwaitinfo) // 346
DECL_TEMPLATE(freebsd, sys___acl_get_file) // 347
DECL_TEMPLATE(freebsd, sys___acl_set_file) // 348
DECL_TEMPLATE(freebsd, sys___acl_get_fd) // // 349
DECL_TEMPLATE(freebsd, sys___acl_set_fd) // 350
DECL_TEMPLATE(freebsd, sys___acl_delete_file) // 351
DECL_TEMPLATE(freebsd, sys___acl_delete_fd) // 352
DECL_TEMPLATE(freebsd, sys___acl_aclcheck_file) // 353
DECL_TEMPLATE(freebsd, sys___acl_aclcheck_fd) // 354

DECL_TEMPLATE(freebsd, sys_extattrctl) // 355
DECL_TEMPLATE(freebsd, sys_extattr_set_file) // 356
DECL_TEMPLATE(freebsd, sys_extattr_get_file) // 357
DECL_TEMPLATE(freebsd, sys_extattr_delete_file) // 358
DECL_TEMPLATE(freebsd, sys_aio_waitcomplete) // 350
DECL_TEMPLATE(freebsd, sys_getresuid) // 360
DECL_TEMPLATE(freebsd, sys_getresgid) // 361
DECL_TEMPLATE(freebsd, sys_kqueue) // 362

#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_kevent) // 363
#else
DECL_TEMPLATE(freebsd, sys_kevent) // 363
#endif
DECL_TEMPLATE(freebsd, sys_extattr_set_fd) // 371
DECL_TEMPLATE(freebsd, sys_extattr_get_fd) // 372
DECL_TEMPLATE(freebsd, sys_extattr_delete_fd) // 373
DECL_TEMPLATE(freebsd, sys___setugid) // 374
DECL_TEMPLATE(freebsd, sys_eaccess) // 376
// unimpl afs3_syscall 377
DECL_TEMPLATE(freebsd, sys_nmount) // 378
// unimpl __mac_get_proc 384
// unimpl __mac_set_proc 385
// unimpl __mac_get_fd 386
// unimpl __mac_get_file 387
// unimpl __mac_set_fd 388
// unimpl __mac_set_file 389
DECL_TEMPLATE(freebsd, sys_kenv) // 390
DECL_TEMPLATE(freebsd, sys_lchflags) // 391
DECL_TEMPLATE(freebsd, sys_uuidgen) // 392
DECL_TEMPLATE(freebsd, sys_sendfile)  // 292

#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_getfsstat) // 395
DECL_TEMPLATE(freebsd, sys_freebsd11_statfs) // 396
DECL_TEMPLATE(freebsd, sys_freebsd11_fstatfs) // 397
DECL_TEMPLATE(freebsd, sys_freebsd11_fhstatfs) // 398
#else
DECL_TEMPLATE(freebsd, sys_getfsstat) // 395
DECL_TEMPLATE(freebsd, sys_statfs) // 396
DECL_TEMPLATE(freebsd, sys_fstatfs) // 397
DECL_TEMPLATE(freebsd, sys_fhstatfs) // 398
#endif

// unimpl ksem_close 400
// unimpl ksem_post 401
// unimpl ksem_wait 402
// unimpl ksem_trywait 403

// unimpl ksem_init 404
// unimpl ksem_open 405
// unimpl ksem_unlink 406
// unimpl ksem_getvalue 407

// unimpl ksem_destroy 408
// unimpl __mac_get_pid 409
// unimpl __mac_get_link 410
// unimpl __mac_set_link 411

DECL_TEMPLATE(freebsd, sys_extattr_set_link) // 412
DECL_TEMPLATE(freebsd, sys_extattr_get_link) // 413
DECL_TEMPLATE(freebsd, sys_extattr_delete_link) // 414
// unimpl __mac_execve 415

DECL_TEMPLATE(freebsd, sys_sigaction) // 416
DECL_TEMPLATE(freebsd, sys_sigreturn) // 417
DECL_TEMPLATE(freebsd, sys_getcontext) // 421
DECL_TEMPLATE(freebsd, sys_setcontext) // 422
DECL_TEMPLATE(freebsd, sys_swapcontext) // 423

#if (FREEBSD_VERS >= FREEBSD_13_1)
DECL_TEMPLATE(freebsd, sys_freebsd13_swapoff) // 424
#else
DECL_TEMPLATE(freebsd, sys_swapoff) // 424
#endif

DECL_TEMPLATE(freebsd, sys___acl_get_link) // 425
DECL_TEMPLATE(freebsd, sys___acl_set_link) // 426
DECL_TEMPLATE(freebsd, sys___acl_delete_link) // 427
DECL_TEMPLATE(freebsd, sys___acl_aclcheck_link) // 428
DECL_TEMPLATE(freebsd, sys_sigwait) // 429
DECL_TEMPLATE(freebsd, sys_thr_create) // 430
DECL_TEMPLATE(freebsd, sys_thr_exit) // 431
DECL_TEMPLATE(freebsd, sys_thr_self) // 432
DECL_TEMPLATE(freebsd, sys_thr_kill) // 433
DECL_TEMPLATE(freebsd, sys__umtx_lock) // 434
DECL_TEMPLATE(freebsd, sys__umtx_unlock) // 435
DECL_TEMPLATE(freebsd, sys_jail_attach) // 436
DECL_TEMPLATE(freebsd, sys_extattr_list_fd) // 437
DECL_TEMPLATE(freebsd, sys_extattr_list_file) // 438
DECL_TEMPLATE(freebsd, sys_extattr_list_link) // 439
// unimpl ksem_timedwait 441
DECL_TEMPLATE(freebsd, sys_thr_suspend) // 442
DECL_TEMPLATE(freebsd, sys_thr_wake) // 443
DECL_TEMPLATE(freebsd, sys_kldunloadf) // 444
// unimpl audit 445
// unimpl auditon 446
// unimpl getauid 447
// unimpl setauid 448
// unimpl getaudit 449
// unimpl setaudit 450
// unimpl getaudit_addr 451
// unimpl setaudit_addr 452
// unimpl auditctl 453
DECL_TEMPLATE(freebsd, sys__umtx_op) // 454
DECL_TEMPLATE(freebsd, sys_thr_new) // 455
DECL_TEMPLATE(freebsd, sys_sigqueue) // 455
DECL_TEMPLATE(freebsd, sys_kmq_open) // 457
DECL_TEMPLATE(freebsd, sys_kmq_setattr) // 458
DECL_TEMPLATE(freebsd, sys_kmq_timedreceive) // 459
DECL_TEMPLATE(freebsd, sys_kmq_timedsend) // 460
DECL_TEMPLATE(freebsd, sys_kmq_notify) // 461
DECL_TEMPLATE(freebsd, sys_kmq_unlink) // 462
DECL_TEMPLATE(freebsd, sys_abort2) // 463
DECL_TEMPLATE(freebsd, sys_thr_set_name) // 464
DECL_TEMPLATE(freebsd, sys_aio_fsync) // 465
DECL_TEMPLATE(freebsd, sys_rtprio_thread) // 466
DECL_TEMPLATE(freebsd, sys_sctp_generic_sendmsg) // 472
DECL_TEMPLATE(freebsd, sys_sctp_generic_recvmsg) // 474
DECL_TEMPLATE(freebsd, sys_pread) // 475
DECL_TEMPLATE(freebsd, sys_pwrite) // 476
DECL_TEMPLATE(freebsd, sys_mmap) // 477
DECL_TEMPLATE(freebsd, sys_lseek) // 478
DECL_TEMPLATE(freebsd, sys_truncate) // 479
DECL_TEMPLATE(freebsd, sys_ftruncate) // 480
DECL_TEMPLATE(freebsd, sys_thr_kill2) // 481
DECL_TEMPLATE(freebsd, sys_shm_open) // 482
DECL_TEMPLATE(freebsd, sys_shm_unlink) // 483
DECL_TEMPLATE(freebsd, sys_cpuset) // 484
DECL_TEMPLATE(freebsd, sys_cpuset_setid) // 485
DECL_TEMPLATE(freebsd, sys_cpuset_getid) // 486
DECL_TEMPLATE(freebsd, sys_cpuset_getaffinity) // 487
DECL_TEMPLATE(freebsd, sys_cpuset_setaffinity) // 488
DECL_TEMPLATE(freebsd, sys_faccessat) // 489
DECL_TEMPLATE(freebsd, sys_fchmodat) //490
DECL_TEMPLATE(freebsd, sys_fchownat) // 491
DECL_TEMPLATE(freebsd, sys_fexecve) // 492
#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_fstatat) // 493
#else
DECL_TEMPLATE(freebsd, sys_fstatat) // 493
#endif
DECL_TEMPLATE(freebsd, sys_futimesat) // 494
DECL_TEMPLATE(freebsd, sys_linkat) // 495
DECL_TEMPLATE(freebsd, sys_mkdirat) // 496
DECL_TEMPLATE(freebsd, sys_mkfifoat) // 497

#if (FREEBSD_VERS >= FREEBSD_12)
DECL_TEMPLATE(freebsd, sys_freebsd11_mknodat) // 498
#else
DECL_TEMPLATE(freebsd, sys_mknodat) // 498
#endif

DECL_TEMPLATE(freebsd, sys_openat) // 499
DECL_TEMPLATE(freebsd, sys_readlinkat) // 500
DECL_TEMPLATE(freebsd, sys_renameat) // 501
DECL_TEMPLATE(freebsd, sys_symlinkat) // 502
DECL_TEMPLATE(freebsd, sys_unlinkat) // 503
DECL_TEMPLATE(freebsd, sys_posix_openpt) // 504
// unimp gssd_syscall 505
DECL_TEMPLATE(freebsd, sys_jail_get) // 506
DECL_TEMPLATE(freebsd, sys_jail_set) // 507
DECL_TEMPLATE(freebsd, sys_jail_remove) // 508
DECL_TEMPLATE(freebsd, sys_closefrom) // 508
DECL_TEMPLATE(freebsd, sys___semctl) // 510
DECL_TEMPLATE(freebsd, sys_msgctl) // 511
DECL_TEMPLATE(freebsd, sys_shmctl) // 512
DECL_TEMPLATE(freebsd, sys_lpathconf) // 513
DECL_TEMPLATE(freebsd, sys_cap_rights_get) // 515
DECL_TEMPLATE(freebsd, sys_cap_enter) // 516
DECL_TEMPLATE(freebsd, sys_cap_getmode) // 517
DECL_TEMPLATE(freebsd, sys_pdfork) // 518
DECL_TEMPLATE(freebsd, sys_pdkill) // 519
DECL_TEMPLATE(freebsd, sys_pdgetpid) // 520
DECL_TEMPLATE(freebsd, sys_pselect) // 522
DECL_TEMPLATE(freebsd, sys_getloginclass) // 523
DECL_TEMPLATE(freebsd, sys_setloginclass) // 524
DECL_TEMPLATE(freebsd, sys_rctl_get_racct) // 525
DECL_TEMPLATE(freebsd, sys_rctl_get_rules) // 526
DECL_TEMPLATE(freebsd, sys_rctl_get_limits) // 527
DECL_TEMPLATE(freebsd, sys_rctl_add_rule) // 528
DECL_TEMPLATE(freebsd, sys_rctl_remove_rule) // 529
DECL_TEMPLATE(freebsd, sys_posix_fallocate) // 530
DECL_TEMPLATE(freebsd, sys_posix_fadvise) // 531
DECL_TEMPLATE(freebsd, sys_wait6) // 532
DECL_TEMPLATE(freebsd, sys_cap_rights_limit) // 533
DECL_TEMPLATE(freebsd, sys_cap_ioctls_limit) // 534
DECL_TEMPLATE(freebsd, sys_cap_ioctls_get) // 535
DECL_TEMPLATE(freebsd, sys_cap_fcntls_limit) // 536
DECL_TEMPLATE(freebsd, sys_cap_fcntls_get) // 537
DECL_TEMPLATE(freebsd, sys_bindat) // 538
DECL_TEMPLATE(freebsd, sys_connectat) // 539
DECL_TEMPLATE(freebsd, sys_chflagsat) // 540
DECL_TEMPLATE(freebsd, sys_accept4) // 541
DECL_TEMPLATE(freebsd, sys_pipe2) // 542
DECL_TEMPLATE(freebsd, sys_aio_mlock) // 543
DECL_TEMPLATE(freebsd, sys_procctl) // 544
DECL_TEMPLATE(freebsd, sys_ppoll) // 545
DECL_TEMPLATE(freebsd, sys_futimens) // 546
DECL_TEMPLATE(freebsd, sys_utimensat) // 547
DECL_TEMPLATE(freebsd, sys_fdatasync) // 550

#if (FREEBSD_VERS >= FREEBSD_12)

DECL_TEMPLATE(freebsd, sys_fstat) // 551
DECL_TEMPLATE(freebsd, sys_fstatat) // 552
DECL_TEMPLATE(freebsd, sys_fhstat) // 553
DECL_TEMPLATE(freebsd, sys_getdirentries) // 554
DECL_TEMPLATE(freebsd, sys_statfs) // 555
DECL_TEMPLATE(freebsd, sys_fstatfs) // 556
DECL_TEMPLATE(freebsd, sys_getfsstat) // 557
DECL_TEMPLATE(freebsd, sys_fhstatfs) // 558
DECL_TEMPLATE(freebsd, sys_mknodat) // 559
DECL_TEMPLATE(freebsd, sys_kevent) // 560
DECL_TEMPLATE(freebsd, sys_cpuset_getdomain) // 561
DECL_TEMPLATE(freebsd, sys_cpuset_setdomain) // 562
DECL_TEMPLATE(freebsd, sys_getrandom) // 563
DECL_TEMPLATE(freebsd, sys_getfhat) // 654
DECL_TEMPLATE(freebsd, sys_fhlink) // 565
DECL_TEMPLATE(freebsd, sys_fhlinkat) // 566
DECL_TEMPLATE(freebsd, sys_fhreadlink) // 567

#endif

#if (FREEBSD_VERS >= FREEBSD_12_2)

DECL_TEMPLATE(freebsd, sys_funlinkat) // 568
DECL_TEMPLATE(freebsd, sys_copy_file_range) // 569
DECL_TEMPLATE(freebsd, sys___sysctlbyname) // 570

#if (FREEBSD_VERS >= FREEBSD_13_0)
// looks like close_range got backported
// to 12.2 leaving these 4 marked as UNIMPL in 12.2
DECL_TEMPLATE(freebsd, sys_shm_open2) // 571
// unimpl __NR_shm_rename          572
DECL_TEMPLATE(freebsd, sys_sigfastblock) // 573
DECL_TEMPLATE(freebsd, sys___realpathat) // 574
#endif

DECL_TEMPLATE(freebsd, sys_close_range)  // 575

#endif

#if (FREEBSD_VERS >= FREEBSD_13_0)

// unimpl __NR_rpctls_syscall      576
DECL_TEMPLATE(freebsd, sys___specialfd) // 577
DECL_TEMPLATE(freebsd, sys_aio_writev)  // 578
DECL_TEMPLATE(freebsd, sys_aio_readv)   // 579

#endif

#if (FREEBSD_VERS >= FREEBSD_13_1)

#if (FREEBSD_VERS >= FREEBSD_14)
// there was a hole in the numbering
DECL_TEMPLATE(freebsd, sys_fspacectl) // 580
#endif
// unimpl __NR_sched_getcpu        581
DECL_TEMPLATE(freebsd, sys_swapoff) // 582
#endif

#if (FREEBSD_VERS >= FREEBSD_15) || (FREEBSD_VERS >= FREEBSD_13_3)
DECL_TEMPLATE(freebsd, sys_kqueuex) // 583
DECL_TEMPLATE(freebsd, sys_membarrier) // 584
#endif

#if (FREEBSD_VERS >= FREEBSD_15)
DECL_TEMPLATE(freebsd, sys_timerfd_create) // 585
DECL_TEMPLATE(freebsd, sys_timerfd_gettime) // 586
DECL_TEMPLATE(freebsd, sys_timerfd_settime) // 587
#endif

DECL_TEMPLATE(freebsd, sys_fake_sigreturn)

#endif   // PRIV_SYSWRAP_FREEBSD_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
