SUBDIRS =  ${VG_ARCH} .

noinst_SCRIPTS = filter_none_discards filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	args.stderr.exp args.stdout.exp args.vgtest \
	bitfield1.stderr.exp bitfield1.vgtest \
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
	exec-sigmask.vgtest exec-sigmask.stdout.exp exec-sigmask.stderr.exp \
	execve.vgtest execve.stdout.exp execve.stderr.exp \
	fcntl_setown.vgtest fcntl_setown.stdout.exp fcntl_setown.stderr.exp \
	floored.stderr.exp floored.stdout.exp \
	floored.vgtest \
	fork.stderr.exp fork.stdout.exp fork.vgtest \
	fucomip.stderr.exp fucomip.vgtest \
	gxx304.stderr.exp gxx304.vgtest \
	map_unmap.stderr.exp map_unmap.stdout.exp map_unmap.vgtest \
	mq.stderr.exp mq.vgtest \
	mremap.stderr.exp mremap.stdout.exp mremap.vgtest \
	munmap_exe.stderr.exp munmap_exe.vgtest \
	pth_blockedsig.stderr.exp \
	pth_blockedsig.stdout.exp pth_blockedsig.vgtest \
	rcrl.stderr.exp rcrl.stdout.exp rcrl.vgtest \
	readline1.stderr.exp readline1.stdout.exp \
	readline1.vgtest \
	resolv.stderr.exp resolv.stdout.exp resolv.vgtest \
	rlimit_nofile.stderr.exp rlimit_nofile.stdout.exp rlimit_nofile.vgtest \
	sem.stderr.exp sem.stdout.exp sem.vgtest \
	semlimit.stderr.exp semlimit.stdout.exp semlimit.vgtest \
	susphello.stdout.exp susphello.stderr.exp susphello.vgtest \
	sha1_test.stderr.exp sha1_test.vgtest \
	shortpush.stderr.exp shortpush.vgtest \
	shorts.stderr.exp shorts.vgtest \
	tls.stderr.exp tls.stdout.exp  \
	smc1.stderr.exp smc1.stdout.exp smc1.vgtest \
	syscall-restart1.vgtest syscall-restart1.stdout.exp syscall-restart1.stderr.exp \
	syscall-restart2.vgtest syscall-restart2.stdout.exp syscall-restart2.stderr.exp \
	system.stderr.exp system.vgtest \
	yield.stderr.exp yield.stdout.exp yield.vgtest

check_PROGRAMS = \
	args bitfield1 closeall coolo_strlen \
	discard exec-sigmask execve fcntl_setown floored fork \
	fucomip \
	munmap_exe map_unmap mq mremap rcrl readline1 \
	resolv rlimit_nofile sem semlimit sha1_test \
	shortpush shorts smc1 susphello pth_blockedsig \
	syscall-restart1 syscall-restart2 system \
	coolo_sigaction gxx304 yield

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
args_SOURCES 		= args.c
bitfield1_SOURCES 	= bitfield1.c
closeall_SOURCES	= closeall.c
coolo_strlen_SOURCES 	= coolo_strlen.c
discard_SOURCES 	= discard.c
exec_sigmask_SOURCES	= exec-sigmask.c
execve_SOURCES		= execve.c
fcntl_setown_SOURCES	= fcntl_setown.c
fork_SOURCES 		= fork.c
floored_SOURCES 	= floored.c
floored_LDADD 		= -lm
fucomip_SOURCES 	= fucomip.c
map_unmap_SOURCES	= map_unmap.c
mq_SOURCES		= mq.c
mq_LDADD		= -lrt
mremap_SOURCES		= mremap.c
munmap_exe_SOURCES 	= munmap_exe.c
rcrl_SOURCES 		= rcrl.c
readline1_SOURCES 	= readline1.c
resolv_SOURCES 		= resolv.c
rlimit_nofile_SOURCES	= rlimit_nofile.c
sem_SOURCES		= sem.c
semlimit_SOURCES	= semlimit.c
semlimit_LDADD		= -lpthread
smc1_SOURCES 		= smc1.c
sha1_test_SOURCES 	= sha1_test.c
shortpush_SOURCES 	= shortpush.c
shorts_SOURCES 		= shorts.c
susphello_SOURCES	= susphello.c
susphello_LDADD		= -lpthread
syscall_restart1_SOURCES = syscall-restart1.c
syscall_restart2_SOURCES = syscall-restart2.c
system_SOURCES 		= system.c
#tls_SOURCES		= tls.c tls2.c
#tls_DEPENDENCIES	= tls.so
#tls_LDFLAGS		= -Wl,-rpath,$(srcdir)
#tls_LDADD		= tls.so -lpthread
#tls_so_SOURCES		= tls_so.c
#tls_so_LDADD		= tls2.so
#tls_so_DEPENDENCIES	= tls2.so
#tls_so_LDFLAGS		= -Wl,-rpath,$(srcdir) -shared
#tls2_so_SOURCES		= tls2_so.c
#tls2_so_LDFLAGS		= -shared
yield_SOURCES		= yield.c
yield_CFLAGS		= $(AM_CFLAGS) -D__$(VG_ARCH)__
yield_LDADD		= -lpthread

# pthread C ones
pth_blockedsig_SOURCES	= pth_blockedsig.c
pth_blockedsig_LDADD	= -lpthread

# generic C++ ones
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp

# must be built with these flags -- bug only occurred with them
fpu_lazy_eflags.o: CFLAGS += -O2 -mcpu=pentiumpro -march=pentiumpro

