SUBDIRS      = ${VG_ARCH}     .
DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = \
	filter_as_mmap filter_fdleak filter_none_discards filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	ansi.stderr.exp ansi.vgtest \
	args.stderr.exp args.stdout.exp args.vgtest \
	as_mmap.stderr.exp as_mmap.stderr.exp2 as_mmap.vgtest \
	as_shm.stderr.exp as_shm.stdout.exp as_shm.vgtest \
	async-sigs.stderr.exp async-sigs.stdout.exp async-sigs.vgtest \
	bitfield1.stderr.exp bitfield1.vgtest \
	blockfault.vgtest blockfault.stderr.exp blockfault.stdout.exp \
	closeall.stderr.exp closeall.vgtest \
	cmdline1.stderr.exp cmdline1.stdout.exp cmdline1.vgtest \
	cmdline2.stderr.exp cmdline2.stdout.exp cmdline2.vgtest \
	cmdline3.stderr.exp cmdline3.vgtest \
	cmdline4.stderr.exp cmdline4.vgtest \
	cmdline5.stderr.exp cmdline5.vgtest \
	cmdline6.stderr.exp cmdline6.vgtest \
	coolo_sigaction.stderr.exp \
	coolo_sigaction.stdout.exp coolo_sigaction.vgtest \
	coolo_strlen.stderr.exp coolo_strlen.vgtest \
	discard.stderr.exp discard.stdout.exp \
	discard.vgtest \
	exec-sigmask.vgtest exec-sigmask.stdout.exp \
	exec-sigmask.stdout.exp2 exec-sigmask.stderr.exp \
	execve.vgtest execve.stdout.exp execve.stderr.exp \
	faultstatus.vgtest faultstatus.stderr.exp \
	fcntl_setown.vgtest fcntl_setown.stdout.exp fcntl_setown.stderr.exp \
	fdleak_cmsg.stderr.exp fdleak_cmsg.stderr.exp2 fdleak_cmsg.stderr.exp3 \
	fdleak_cmsg.vgtest \
	fdleak_creat.stderr.exp fdleak_creat.stderr.exp2 \
	fdleak_creat.stderr.exp3 fdleak_creat.vgtest \
	fdleak_dup.stderr.exp fdleak_dup.stderr.exp2 \
	fdleak_dup.stderr.exp3 fdleak_dup.vgtest \
	fdleak_dup2.stderr.exp fdleak_dup2.stderr.exp2 \
	fdleak_dup2.stderr.exp3 fdleak_dup2.vgtest \
	fdleak_fcntl.stderr.exp fdleak_fcntl.stderr.exp2 \
	fdleak_fcntl.stderr.exp3 fdleak_fcntl.stderr.exp4 \
	fdleak_fcntl.vgtest \
	fdleak_ipv4.stderr.exp fdleak_ipv4.stderr.exp2 \
	fdleak_ipv4.stdout.exp fdleak_ipv4.vgtest \
	fdleak_open.stderr.exp fdleak_open.stderr.exp2 fdleak_open.vgtest \
	fdleak_pipe.stderr.exp fdleak_pipe.stderr.exp2 fdleak_pipe.vgtest \
	fdleak_socketpair.stderr.exp fdleak_socketpair.stderr.exp2 \
	fdleak_socketpair.vgtest \
	floored.stderr.exp floored.stdout.exp \
	floored.vgtest \
	fork.stderr.exp fork.stdout.exp fork.vgtest \
	fucomip.stderr.exp fucomip.vgtest \
	gxx304.stderr.exp gxx304.vgtest \
	manythreads.stdout.exp manythreads.stderr.exp manythreads.vgtest \
	map_unaligned.stderr.exp map_unaligned.vgtest \
	map_unmap.stderr.exp map_unmap.stdout.exp map_unmap.vgtest \
	mq.stderr.exp mq.vgtest \
	mremap.stderr.exp mremap.stdout.exp mremap.vgtest \
	mremap2.stderr.exp mremap2.stdout.exp mremap2.vgtest \
	munmap_exe.stderr.exp munmap_exe.vgtest \
	nestedfns.stderr.exp nestedfns.stdout.exp nestedfns.vgtest \
	pending.stdout.exp pending.stderr.exp pending.vgtest \
	pth_atfork1.stderr.exp pth_atfork1.stdout.exp pth_atfork1.vgtest \
	pth_blockedsig.stderr.exp \
	pth_blockedsig.stdout.exp pth_blockedsig.vgtest \
	pth_cancel1.stderr.exp pth_cancel1.stdout.exp pth_cancel1.vgtest \
	pth_cancel2.stderr.exp pth_cancel2.vgtest \
	pth_cvsimple.stderr.exp pth_cvsimple.stdout.exp pth_cvsimple.vgtest \
	pth_empty.stderr.exp pth_empty.vgtest \
	pth_exit.stderr.exp pth_exit.vgtest \
	pth_exit2.stderr.exp pth_exit2.vgtest \
	pth_mutexspeed.stderr.exp \
	pth_mutexspeed.stdout.exp pth_mutexspeed.vgtest \
	pth_once.stderr.exp pth_once.stdout.exp pth_once.vgtest \
	pth_rwlock.stderr.exp pth_rwlock.vgtest \
	pth_stackalign.stderr.exp \
	pth_stackalign.stdout.exp pth_stackalign.vgtest \
	rcrl.stderr.exp rcrl.stdout.exp rcrl.vgtest \
	readline1.stderr.exp readline1.stdout.exp \
	readline1.vgtest \
	res_search.stderr.exp res_search.stdout.exp res_search.vgtest \
	resolv.stderr.exp resolv.stdout.exp resolv.vgtest \
	rlimit_nofile.stderr.exp rlimit_nofile.stdout.exp rlimit_nofile.vgtest \
	selfrun.stderr.exp selfrun.stdout.exp selfrun.vgtest \
	sem.stderr.exp sem.stdout.exp sem.vgtest \
	semlimit.stderr.exp semlimit.stdout.exp semlimit.vgtest \
	shell shell.vgtest shell.stderr.exp shell.stdout.exp \
	shell_badinterp shell_badinterp.vgtest shell_badinterp.stderr.exp \
	shell_binaryfile shell_binaryfile.vgtest shell_binaryfile.stderr.exp \
	shell_dir.vgtest shell_dir.stderr.exp \
	shell_nonexec.vgtest shell_nonexec.stderr.exp \
	shell_nosuchfile.vgtest shell_nosuchfile.stderr.exp \
	shell_valid1 shell_valid1.vgtest shell_valid1.stderr.exp \
	shell_valid2 shell_valid2.vgtest shell_valid2.stderr.exp \
	shell_valid3 shell_valid3.vgtest shell_valid3.stderr.exp \
	shell_zerolength shell_zerolength.vgtest shell_zerolength.stderr.exp \
	susphello.stdout.exp susphello.stderr.exp susphello.vgtest \
	sha1_test.stderr.exp sha1_test.vgtest \
	shortpush.stderr.exp shortpush.vgtest \
	shorts.stderr.exp shorts.vgtest \
	sigstackgrowth.stdout.exp sigstackgrowth.stderr.exp sigstackgrowth.vgtest \
	stackgrowth.stdout.exp stackgrowth.stderr.exp stackgrowth.vgtest \
	syscall-restart1.vgtest syscall-restart1.stdout.exp syscall-restart1.stderr.exp \
	syscall-restart2.vgtest syscall-restart2.stdout.exp syscall-restart2.stderr.exp \
	system.stderr.exp system.vgtest \
	thread-exits.stderr.exp thread-exits.stdout.exp thread-exits.vgtest \
	threaded-fork.stderr.exp threaded-fork.stdout.exp threaded-fork.vgtest \
	threadederrno.stderr.exp threadederrno.stdout.exp \
	threadederrno.vgtest \
	tls.vgtest tls.stderr.exp tls.stdout.exp  \
	vgprintf.stderr.exp vgprintf.stdout.exp vgprintf.vgtest

check_PROGRAMS = \
	ansi args as_mmap as_shm \
	async-sigs bitfield1 blockfault closeall coolo_strlen \
	discard exec-sigmask execve faultstatus fcntl_setown \
	fdleak_cmsg fdleak_creat fdleak_dup fdleak_dup2 \
	fdleak_fcntl fdleak_ipv4 fdleak_open fdleak_pipe \
	fdleak_socketpair \
	floored fork fucomip manythreads \
	munmap_exe map_unaligned map_unmap mq mremap mremap2 \
	nestedfns \
	pending \
	pth_atfork1 pth_blockedsig pth_cancel1 pth_cancel2 pth_cvsimple \
	pth_empty pth_exit pth_exit2 pth_mutexspeed pth_once pth_rwlock \
	pth_stackalign \
	rcrl readline1 res_search resolv \
	rlimit_nofile selfrun sem semlimit sha1_test \
	shortpush shorts stackgrowth sigstackgrowth susphello \
	syscall-restart1 syscall-restart2 system \
	thread-exits threaded-fork threadederrno \
	tls tls.so tls2.so vgprintf \
	coolo_sigaction gxx304

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# Extra stuff for C tests
ansi_CFLAGS		= -ansi
floored_LDADD 		= -lm
manythreads_LDADD	= -lpthread
mq_LDADD		= -lrt
pth_atfork1_LDADD	= -lpthread
pth_blockedsig_LDADD	= -lpthread
pth_cancel1_LDADD	= -lpthread
pth_cancel2_LDADD	= -lpthread
pth_cvsimple_LDADD	= -lpthread
pth_empty_LDADD 	= -lpthread
pth_exit_LDADD	 	= -lpthread
pth_exit2_LDADD	 	= -lpthread
pth_mutexspeed_LDADD	= -lpthread
pth_once_LDADD		= -lpthread
pth_rwlock_LDADD	= -lpthread
pth_stackalign_LDADD	= -lpthread
res_search_LDADD        = -lresolv -lpthread
semlimit_LDADD		= -lpthread
susphello_LDADD		= -lpthread
thread_exits_LDADD	= -lpthread
threaded_fork_LDADD	= -lpthread
threadederrno_LDADD	= -lpthread
tls_SOURCES		= tls.c tls2.c
tls_DEPENDENCIES	= tls.so
tls_LDFLAGS		= -Wl,-rpath,$(top_builddir)/none/tests
tls_LDADD		= tls.so -lpthread
tls_so_SOURCES		= tls_so.c
tls_so_LDADD		= tls2.so
tls_so_DEPENDENCIES	= tls2.so
tls_so_LDFLAGS		= -Wl,-rpath,$(top_builddir)/none/tests -shared -fPIC
tls_so_CFLAGS		= -fPIC
tls2_so_SOURCES		= tls2_so.c
tls2_so_LDFLAGS		= -shared

# C++ tests
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp

