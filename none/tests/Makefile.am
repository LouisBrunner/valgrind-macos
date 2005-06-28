SUBDIRS      = ${VG_ARCH}     .
DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_none_discards filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	ansi.stderr.exp ansi.vgtest \
	args.stderr.exp args.stdout.exp args.vgtest \
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
	munmap_exe.stderr.exp munmap_exe.vgtest \
	pending.stdout.exp pending.stderr.exp pending.vgtest \
	pth_blockedsig.stderr.exp \
	pth_blockedsig.stdout.exp pth_blockedsig.vgtest \
	pth_stackalign.stderr.exp \
	pth_stackalign.stdout.exp pth_stackalign.vgtest \
	rcrl.stderr.exp rcrl.stdout.exp rcrl.vgtest \
	readline1.stderr.exp readline1.stdout.exp \
	readline1.vgtest \
	resolv.stderr.exp resolv.stdout.exp resolv.vgtest \
	rlimit_nofile.stderr.exp rlimit_nofile.stdout.exp rlimit_nofile.vgtest \
	selfrun.stderr.exp selfrun.stdout.exp selfrun.vgtest \
	sem.stderr.exp sem.stdout.exp sem.vgtest \
	semlimit.stderr.exp semlimit.stdout.exp semlimit.vgtest \
	susphello.stdout.exp susphello.stderr.exp susphello.vgtest \
	sha1_test.stderr.exp sha1_test.vgtest \
	shortpush.stderr.exp shortpush.vgtest \
	shorts.stderr.exp shorts.vgtest \
	sigstackgrowth.stdout.exp sigstackgrowth.stderr.exp sigstackgrowth.vgtest \
	smc1.stderr.exp smc1.stdout.exp smc1.vgtest \
	stackgrowth.stdout.exp stackgrowth.stderr.exp stackgrowth.vgtest \
	syscall-restart1.vgtest syscall-restart1.stdout.exp syscall-restart1.stderr.exp \
	syscall-restart2.vgtest syscall-restart2.stdout.exp syscall-restart2.stderr.exp \
	system.stderr.exp system.vgtest \
	thread-exits.stderr.exp thread-exits.stdout.exp thread-exits.vgtest \
	threaded-fork.stderr.exp threaded-fork.stdout.exp threaded-fork.vgtest \
	tls.stderr.exp tls.stdout.exp  \
	yield.stderr.exp yield.stdout.exp yield.vgtest

check_PROGRAMS = \
	ansi args async-sigs bitfield1 blockfault closeall coolo_strlen \
	discard exec-sigmask execve faultstatus fcntl_setown floored fork \
	fucomip \
	manythreads \
	munmap_exe map_unaligned map_unmap mq mremap rcrl readline1 \
	resolv rlimit_nofile selfrun sem semlimit sha1_test \
	shortpush shorts \
	stackgrowth sigstackgrowth \
	smc1 susphello pending pth_blockedsig pth_stackalign \
	syscall-restart1 syscall-restart2 system \
	thread-exits threaded-fork \
	tls tls.so tls2.so \
	coolo_sigaction gxx304 yield

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
ansi_SOURCES		= ansi.c
ansi_CFLAGS		= -ansi
args_SOURCES 		= args.c
async_sigs_SOURCES	= async-sigs.c
bitfield1_SOURCES 	= bitfield1.c
blockfault_SOURCES	= blockfault.c
closeall_SOURCES	= closeall.c
coolo_strlen_SOURCES 	= coolo_strlen.c
discard_SOURCES 	= discard.c
exec_sigmask_SOURCES	= exec-sigmask.c
execve_SOURCES		= execve.c
faultstatus_SOURCES	= faultstatus.c
fcntl_setown_SOURCES	= fcntl_setown.c
fork_SOURCES 		= fork.c
floored_SOURCES 	= floored.c
floored_LDADD 		= -lm
fucomip_SOURCES 	= fucomip.c
pending_SOURCES		= pending.c
manythreads_SOURCES	= manythreads.c
manythreads_LDADD	= -lpthread
map_unaligned_SOURCES	= map_unaligned.c
map_unmap_SOURCES	= map_unmap.c
mq_SOURCES		= mq.c
mq_LDADD		= -lrt
mremap_SOURCES		= mremap.c
munmap_exe_SOURCES 	= munmap_exe.c
pth_blockedsig_SOURCES	= pth_blockedsig.c
pth_blockedsig_LDADD	= -lpthread
pth_stackalign_SOURCES	= pth_stackalign.c
pth_stackalign_LDADD	= -lpthread
rcrl_SOURCES 		= rcrl.c
readline1_SOURCES 	= readline1.c
resolv_SOURCES 		= resolv.c
rlimit_nofile_SOURCES	= rlimit_nofile.c
selfrun_SOURCES		= selfrun.c
sem_SOURCES		= sem.c
semlimit_SOURCES	= semlimit.c
semlimit_LDADD		= -lpthread
smc1_SOURCES 		= smc1.c
sha1_test_SOURCES 	= sha1_test.c
shortpush_SOURCES 	= shortpush.c
shorts_SOURCES 		= shorts.c
sigstackgrowth_SOURCES	= sigstackgrowth.c
susphello_SOURCES	= susphello.c
susphello_LDADD		= -lpthread
stackgrowth_SOURCES	= stackgrowth.c
syscall_restart1_SOURCES = syscall-restart1.c
syscall_restart2_SOURCES = syscall-restart2.c
system_SOURCES 		= system.c
thread_exits_SOURCES	= thread-exits.c
thread_exits_LDADD	= -lpthread
threaded_fork_SOURCES	= threaded-fork.c
threaded_fork_LDADD	= -lpthread
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
yield_SOURCES		= yield.c
yield_CFLAGS		= $(AM_CFLAGS) -D__$(VG_ARCH)__
yield_LDADD		= -lpthread

# generic C++ ones
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp

# must be built with these flags -- bug only occurred with them
fpu_lazy_eflags.o: CFLAGS += -O2 -mcpu=pentiumpro -march=pentiumpro

