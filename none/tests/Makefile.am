
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .

# Arch-specific tests.
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif
if VGCONF_ARCHS_INCLUDE_AMD64
SUBDIRS += amd64
endif
if VGCONF_ARCHS_INCLUDE_PPC32
SUBDIRS += ppc32
endif
if VGCONF_ARCHS_INCLUDE_PPC64
SUBDIRS += ppc64
endif
if VGCONF_ARCHS_INCLUDE_ARM
SUBDIRS += arm
endif

# OS-specific tests
if VGCONF_OS_IS_LINUX
SUBDIRS += linux
endif
if VGCONF_OS_IS_DARWIN
SUBDIRS += darwin
endif

# Platform-specific tests
if VGCONF_PLATFORMS_INCLUDE_X86_LINUX
SUBDIRS += x86-linux
endif

DIST_SUBDIRS = x86 amd64 ppc32 ppc64 arm linux darwin x86-linux .

dist_noinst_SCRIPTS = \
	filter_cmdline0 \
	filter_cmdline1 \
	filter_fdleak \
	filter_linenos \
	filter_none_discards \
	filter_stderr \
	filter_timestamp

noinst_HEADERS = fdleak.h

EXTRA_DIST = \
	ansi.stderr.exp ansi.vgtest \
	args.stderr.exp args.stdout.exp args.vgtest \
	async-sigs.stderr.exp async-sigs.vgtest \
	bitfield1.stderr.exp bitfield1.vgtest \
	bug129866.vgtest bug129866.stderr.exp bug129866.stdout.exp \
	closeall.stderr.exp closeall.vgtest \
	cmdline0.stderr.exp cmdline0.stdout.exp cmdline0.vgtest \
	cmdline1.stderr.exp cmdline1.stdout.exp cmdline1.vgtest \
	cmdline2.stderr.exp cmdline2.stdout.exp cmdline2.vgtest \
	cmdline3.stderr.exp cmdline3.vgtest \
	cmdline4.stderr.exp cmdline4.vgtest \
	cmdline5.stderr.exp cmdline5.vgtest \
	cmdline6.stderr.exp cmdline6.vgtest \
	cmd-with-special.stderr.exp cmd-with-special.vgtest \
	coolo_sigaction.stderr.exp \
	coolo_sigaction.stdout.exp coolo_sigaction.vgtest \
	coolo_strlen.stderr.exp coolo_strlen.vgtest \
	discard.stderr.exp discard.stdout.exp \
	discard.vgtest \
	exec-sigmask.vgtest exec-sigmask.stdout.exp \
	exec-sigmask.stdout.exp2 exec-sigmask.stdout.exp3 exec-sigmask.stderr.exp \
	execve.vgtest execve.stdout.exp execve.stderr.exp \
	faultstatus.vgtest faultstatus.stderr.exp \
	fcntl_setown.vgtest fcntl_setown.stdout.exp fcntl_setown.stderr.exp \
	fdleak_cmsg.stderr.exp fdleak_cmsg.vgtest \
	fdleak_creat.stderr.exp fdleak_creat.vgtest \
	fdleak_dup.stderr.exp fdleak_dup.vgtest \
	fdleak_dup2.stderr.exp fdleak_dup2.vgtest \
	fdleak_fcntl.stderr.exp fdleak_fcntl.vgtest \
	fdleak_ipv4.stderr.exp fdleak_ipv4.stdout.exp fdleak_ipv4.vgtest \
	fdleak_open.stderr.exp fdleak_open.vgtest \
	fdleak_pipe.stderr.exp fdleak_pipe.vgtest \
	fdleak_socketpair.stderr.exp fdleak_socketpair.vgtest \
	floored.stderr.exp floored.stdout.exp floored.vgtest \
	fork.stderr.exp fork.stdout.exp fork.vgtest \
	fucomip.stderr.exp fucomip.vgtest \
	gxx304.stderr.exp gxx304.vgtest \
	manythreads.stdout.exp manythreads.stderr.exp manythreads.vgtest \
	map_unaligned.stderr.exp map_unaligned.vgtest \
	map_unmap.stderr.exp map_unmap.stdout.exp map_unmap.vgtest \
	mq.stderr.exp mq.vgtest \
	munmap_exe.stderr.exp munmap_exe.vgtest \
	nestedfns.stderr.exp nestedfns.stdout.exp nestedfns.vgtest \
	pending.stdout.exp pending.stderr.exp pending.vgtest \
	procfs-linux.stderr.exp-with-readlinkat \
	procfs-linux.stderr.exp-without-readlinkat \
	procfs-linux.vgtest \
	procfs-non-linux.stderr.exp procfs-non-linux.vgtest \
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
	require-text-symbol-1.vgtest \
		require-text-symbol-1.stderr.exp \
	require-text-symbol-2.vgtest \
		require-text-symbol-2.stderr.exp-libcso6 \
	res_search.stderr.exp res_search.stdout.exp res_search.vgtest \
	resolv.stderr.exp resolv.stdout.exp resolv.vgtest \
	rlimit_nofile.stderr.exp rlimit_nofile.stdout.exp rlimit_nofile.vgtest \
	selfrun.stderr.exp selfrun.stdout.exp selfrun.vgtest \
	sem.stderr.exp sem.stdout.exp sem.vgtest \
	semlimit.stderr.exp semlimit.stdout.exp semlimit.vgtest \
	shell shell.vgtest shell.stderr.exp shell.stderr.exp-dash \
	    shell.stdout.exp \
	shell_badinterp shell_badinterp.vgtest shell_badinterp.stderr.exp \
	shell_binaryfile shell_binaryfile.vgtest shell_binaryfile.stderr.exp \
	shell_dir.vgtest shell_dir.stderr.exp \
	shell_nonexec.vgtest shell_nonexec.stderr.exp \
	shell_nosuchfile.vgtest shell_nosuchfile.stderr.exp \
	shell_valid1 shell_valid1.vgtest shell_valid1.stderr.exp \
	shell_valid2 shell_valid2.vgtest shell_valid2.stderr.exp \
	shell_valid3 shell_valid3.vgtest shell_valid3.stderr.exp \
	shell_zerolength shell_zerolength.vgtest shell_zerolength.stderr.exp \
	    shell_zerolength.stderr.exp-dash \
	sha1_test.stderr.exp sha1_test.vgtest \
	shortpush.stderr.exp shortpush.vgtest \
	shorts.stderr.exp shorts.vgtest \
	sigstackgrowth.stdout.exp sigstackgrowth.stderr.exp sigstackgrowth.vgtest \
	stackgrowth.stdout.exp stackgrowth.stderr.exp stackgrowth.vgtest \
	syscall-restart1.vgtest syscall-restart1.stdout.exp syscall-restart1.stderr.exp \
	syscall-restart2.vgtest syscall-restart2.stdout.exp syscall-restart2.stderr.exp \
	syslog.vgtest syslog.stderr.exp \
	system.stderr.exp system.vgtest \
	thread-exits.stderr.exp thread-exits.stdout.exp thread-exits.vgtest \
	threaded-fork.stderr.exp threaded-fork.stdout.exp threaded-fork.vgtest \
	threadederrno.stderr.exp threadederrno.stdout.exp \
	threadederrno.vgtest \
	timestamp.stderr.exp timestamp.vgtest \
	tls.vgtest tls.stderr.exp tls.stdout.exp  \
	vgprintf.stderr.exp vgprintf.vgtest

check_PROGRAMS = \
	ansi args \
	async-sigs \
	bitfield1 \
	bug129866 \
	closeall coolo_strlen \
	discard exec-sigmask execve faultstatus fcntl_setown \
	fdleak_cmsg fdleak_creat fdleak_dup fdleak_dup2 \
	fdleak_fcntl fdleak_ipv4 fdleak_open fdleak_pipe \
	fdleak_socketpair \
	floored fork fucomip \
	munmap_exe map_unaligned map_unmap mq \
	nestedfns \
	pending \
	procfs-cmdline-exe \
	pth_atfork1 pth_blockedsig pth_cancel1 pth_cancel2 pth_cvsimple \
	pth_empty pth_exit pth_exit2 pth_mutexspeed pth_once pth_rwlock \
	pth_stackalign \
	rcrl readline1 \
	require-text-symbol \
	res_search resolv \
	rlimit_nofile selfrun sem semlimit sha1_test \
	shortpush shorts stackgrowth sigstackgrowth \
	syscall-restart1 syscall-restart2 \
	syslog \
	system \
	threaded-fork \
	threadederrno \
	timestamp \
	tls \
	tls.so \
	tls2.so \
	valgrind_cpp_test \
	vgprintf \
	coolo_sigaction \
	gxx304

# DDD:
# - manythreads and thread-exits have lots of this:
# --61831:0:aspacem  sync_check_mapping_callback: segment mismatch:
#                    kernel's seg:
# --61831:0:aspacem  start=0x102538000 end=0x1025b7fff prot=3 dev=0 ino=0 
#                    offset=0 name="(none)"
# --61831:0:aspacem  sync_check_mapping_callback: segment mismatch: V's seg:
# --61831:0:aspacem  NSegment{    , start=0x10067a000, end=0x109a1efff,
#                    smode=SmFixed, dev=0, ino=0, offset=0, fnIdx=-1, hasR=0,
#                    hasW=0, hasX=0, hasT=0, mark=0, name="(none)"}
if ! VGCONF_PLATFORMS_INCLUDE_AMD64_DARWIN
   check_PROGRAMS += \
	manythreads \
	thread-exits
endif


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# Extra stuff for C tests
ansi_CFLAGS		= $(AM_CFLAGS) -ansi
floored_LDADD 		= -lm
manythreads_LDADD	= -lpthread
if VGCONF_OS_IS_DARWIN
 nestedfns_CFLAGS	= $(AM_CFLAGS) -fnested-functions
else
 nestedfns_CFLAGS	= $(AM_CFLAGS)
 mq_LDADD		= -lrt
endif
pth_atfork1_LDADD	= -lpthread
pth_blockedsig_LDADD	= -lpthread
pth_cancel1_CFLAGS	= $(AM_CFLAGS) -Wno-shadow
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
if VGCONF_OS_IS_AIX5
 res_search_LDADD       = -lpthread
else
 res_search_LDADD       = -lresolv -lpthread
 resolv_LDADD           = -lresolv -lpthread
endif
semlimit_LDADD		= -lpthread
thread_exits_LDADD	= -lpthread
threaded_fork_LDADD	= -lpthread
threadederrno_LDADD	= -lpthread
tls_SOURCES		= tls.c tls2.c
tls_DEPENDENCIES	= tls.so tls2.so
if VGCONF_OS_IS_AIX5
 tls_LDFLAGS		= 
else
 tls_LDFLAGS		= -Wl,-rpath,$(top_builddir)/none/tests
endif
tls_LDADD		= tls.so tls2.so -lpthread
tls_so_SOURCES		= tls_so.c
tls_so_DEPENDENCIES	= tls2.so
if VGCONF_OS_IS_AIX5
 tls_so_LDFLAGS		= -shared -fPIC
 tls_so_LDADD		= `pwd`/tls2.so
else
if VGCONF_OS_IS_DARWIN
 tls_so_LDFLAGS		= -dynamic -dynamiclib -all_load -fpic
 tls_so_LDADD		= `pwd`/tls2.so
else
 tls_so_LDFLAGS		= -Wl,-rpath,$(top_builddir)/none/tests -shared -fPIC
 tls_so_LDADD		= tls2.so
endif
endif
tls_so_CFLAGS		= $(AM_CFLAGS) -fPIC
tls2_so_SOURCES		= tls2_so.c
if VGCONF_OS_IS_DARWIN
tls2_so_LDFLAGS		= -dynamic -dynamiclib -all_load
else
tls2_so_LDFLAGS		= -shared
endif

valgrind_cpp_test_SOURCES = valgrind_cpp_test.cpp
valgrind_cpp_test_LDADD   = -lstdc++

# C++ tests
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp

