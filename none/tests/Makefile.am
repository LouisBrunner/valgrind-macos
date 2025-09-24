
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .
SUBDIRS += scripts

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
if VGCONF_ARCHS_INCLUDE_ARM64
SUBDIRS += arm64
endif
if VGCONF_ARCHS_INCLUDE_S390X
SUBDIRS += s390x
endif
if VGCONF_ARCHS_INCLUDE_MIPS32
SUBDIRS += mips32
endif
if VGCONF_ARCHS_INCLUDE_MIPS64
SUBDIRS += mips64
endif
if VGCONF_ARCHS_INCLUDE_NANOMIPS
SUBDIRS += nanomips
endif
if VGCONF_ARCHS_INCLUDE_RISCV64
SUBDIRS += riscv64
endif


# OS-specific tests
if VGCONF_OS_IS_LINUX
SUBDIRS += linux
endif
if VGCONF_OS_IS_DARWIN
SUBDIRS += darwin
endif
if VGCONF_OS_IS_SOLARIS
SUBDIRS += solaris
endif
if VGCONF_OS_IS_FREEBSD
SUBDIRS += freebsd
endif

# Platform-specific tests
if VGCONF_PLATFORMS_INCLUDE_AMD64_LINUX
SUBDIRS += amd64-linux
endif
if VGCONF_PLATFORMS_INCLUDE_X86_LINUX
SUBDIRS += x86-linux
endif
if VGCONF_PLATFORMS_INCLUDE_AMD64_DARWIN
SUBDIRS += amd64-darwin
endif
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
SUBDIRS += x86-darwin
endif
if VGCONF_PLATFORMS_INCLUDE_AMD64_SOLARIS
SUBDIRS += amd64-solaris
endif
if VGCONF_PLATFORMS_INCLUDE_X86_SOLARIS
SUBDIRS += x86-solaris
endif
if VGCONF_PLATFORMS_INCLUDE_X86_FREEBSD
SUBDIRS += x86-freebsd
endif

DIST_SUBDIRS = x86 amd64 ppc32 ppc64 arm arm64 s390x mips32 mips64 nanomips \
               riscv64 linux darwin solaris freebsd amd64-linux x86-linux \
               amd64-darwin x86-darwin amd64-solaris x86-solaris x86-freebsd \
               scripts .

dist_noinst_SCRIPTS = \
	filter_cmdline0 \
	filter_cmdline1 \
	filter_fdleak \
	filter_ioctl_moans \
	filter_none_discards \
	filter_stderr \
	filter_timestamp \
	filter_xml \
	allexec_prepare_prereq

noinst_HEADERS = fdleak.h

EXTRA_DIST = \
	allexec32.stdout.exp allexec32.stderr.exp allexec32.vgtest\
	allexec64.stdout.exp allexec64.stderr.exp allexec64.vgtest\
	ansi.stderr.exp ansi.vgtest \
	args.stderr.exp args.stdout.exp args.vgtest \
	async-sigs.stderr.exp async-sigs.stderr.exp-mips32 \
	async-sigs.vgtest async-sigs.stderr.exp-freebsd \
	bigcode.vgtest bigcode.stderr.exp bigcode.stdout.exp \
	bitfield1.stderr.exp bitfield1.vgtest \
	bug129866.vgtest bug129866.stderr.exp bug129866.stdout.exp \
	bug234814.vgtest bug234814.stderr.exp bug234814.stdout.exp \
	bug290061.vgtest bug290061.stderr.exp \
	bug491394.vgtest bug491394.stderr.exp \
	bug492678.vgtest bug492678.stderr.exp \
	closeall.stderr.exp closeall.vgtest \
	cmdline0.stderr.exp cmdline0.stdout.exp cmdline0.vgtest \
	cmdline1.stderr.exp cmdline1.stdout.exp cmdline1.vgtest \
		cmdline1.stdout.exp-non-linux \
	cmdline2.stderr.exp cmdline2.stdout.exp cmdline2.vgtest \
		cmdline2.stdout.exp-non-linux \
	cmdline3.stderr.exp cmdline3.vgtest \
	cmdline4.stderr.exp cmdline4.vgtest \
	cmdline5.stderr.exp cmdline5.vgtest \
	cmdline6.stderr.exp cmdline6.vgtest \
	cmdline7.stderr.exp cmdline7.vgtest \
	cmdline8.stderr.exp cmdline8.vgtest \
	cmdline9.stderr.exp cmdline9.vgtest \
	cmdline10.stderr.exp cmdline10.vgtest \
	cmdline11.stderr.exp cmdline11.vgtest \
	cmd-with-special.stderr.exp cmd-with-special.vgtest \
	coolo_sigaction.stderr.exp \
	coolo_sigaction.stdout.exp coolo_sigaction.vgtest \
	coolo_strlen.stderr.exp coolo_strlen.vgtest \
	discard.stderr.exp discard.stdout.exp \
	discard.vgtest \
	double_close_range_xml.stderr.exp \
	double_close_range_xml.vgtest \
	double_close_range_sup.stderr.exp double_close_range_sup.vgtest \
	empty-exe.vgtest empty-exe.stderr.exp \
	exec-sigmask.vgtest exec-sigmask.stdout.exp \
	exec-sigmask.stdout.exp2 exec-sigmask.stdout.exp3 \
	exec-sigmask.stdout.exp-solaris exec-sigmask.stderr.exp \
	execve.vgtest execve.stdout.exp execve.stderr.exp \
	faultstatus.vgtest faultstatus.stderr.exp faultstatus.stderr.exp-s390x \
	fcntl_setown.vgtest fcntl_setown.stdout.exp fcntl_setown.stderr.exp \
	fdleak_cat.vgtest fdleak_cat.stderr.exp \
	fdleak_cmsg.stderr.exp fdleak_cmsg.vgtest \
	fdleak_cmsg_xml.stderr.exp-ppc64le \
	fdleak_cmsg_xml.stderr.exp fdleak_cmsg_xml.vgtest \
	fdleak_cmsg_supp.stderr.exp fdleak_cmsg_supp.supp \
	fdleak_cmsg_supp.vgtest \
	fdleak_creat.stderr.exp fdleak_creat.vgtest \
	fdleak_creat_xml.stderr.exp fdleak_creat_xml.vgtest \
	fdleak_creat_sup.stderr.exp fdleak_creat_sup.supp \
	fdleak_creat_sup.vgtest \
	fdleak_doubleclose0.stderr.exp fdleak_doubleclose0.vgtest \
	fdleak_dup.stderr.exp fdleak_dup.vgtest \
	fdleak_dup_xml.stderr.exp fdleak_dup_xml.vgtest \
	fdleak_dup2.stderr.exp fdleak_dup2.vgtest \
	fdleak_dup2_xml.stderr.exp fdleak_dup2_xml.vgtest \
	fdleak_fcntl.stderr.exp fdleak_fcntl.vgtest \
	fdleak_fcntl_xml.stderr.exp fdleak_fcntl_xml.vgtest \
	fdleak_ipv4.stderr.exp fdleak_ipv4.stdout.exp fdleak_ipv4.vgtest \
	fdleak_ipv4_xml.stderr.exp-ppc64be \
	fdleak_ipv4_xml.stderr.exp-ppc64le \
	fdleak_ipv4_xml.stderr.exp fdleak_ipv4_xml.stdout.exp \
	fdleak_ipv4_xml.vgtest fdleak_ipv4_xml.stderr.exp-nomain \
	fdleak_open.stderr.exp fdleak_open.vgtest \
	fdleak_open_xml.stderr.exp fdleak_open_xml.vgtest \
	fdleak_pipe.stderr.exp fdleak_pipe.vgtest \
	fdleak_pipe_xml.stderr.exp fdleak_pipe_xml.vgtest \
	fdleak_socketpair.stderr.exp fdleak_socketpair.vgtest \
	fdleak_socketpair_xml.stderr.exp fdleak_socketpair_xml.vgtest \
		fdleak_socketpair_xml.stderr.exp-illumos \
	floored.stderr.exp floored.stdout.exp floored.vgtest \
	fork.stderr.exp fork.stdout.exp fork.vgtest \
	fucomip.stderr.exp fucomip.vgtest \
	gxx304.stderr.exp gxx304.vgtest \
	ifunc.stderr.exp ifunc.stdout.exp ifunc.vgtest \
	ioctl_moans.stderr.exp ioctl_moans.vgtest \
	libvex_test.stderr.exp libvex_test.vgtest \
	libvexmultiarch_test.stderr.exp libvexmultiarch_test.vgtest \
	manythreads.stdout.exp manythreads.stderr.exp manythreads.vgtest \
	map_unaligned.stderr.exp map_unaligned.vgtest \
	map_unmap.stderr.exp map_unmap.stdout.exp map_unmap.vgtest \
	mmap_fcntl_bug.vgtest mmap_fcntl_bug.stdout.exp \
		mmap_fcntl_bug.stderr.exp \
	mmap_o_direct.stderr.exp mmap_o_direct.vgtest \
	mq.stderr.exp mq.vgtest \
	munmap_exe.stderr.exp munmap_exe.vgtest \
	nestedfns.stderr.exp nestedfns.stdout.exp nestedfns.vgtest \
	nocwd.stdout.exp nocwd.stderr.exp nocwd.vgtest \
	nocwd.stderr.exp-freebsd \
	nodir.stderr.exp nodir.vgtest \
		nodir.stderr.exp-freebsd \
	pending.stdout.exp pending.stderr.exp pending.vgtest \
	ppoll_alarm.stdout.exp ppoll_alarm.stderr.exp ppoll_alarm.vgtest \
	procfs-linux.stderr.exp-with-readlinkat \
	procfs-linux.stderr.exp-without-readlinkat \
	procfs-linux.vgtest \
	procfs-non-linux.vgtest \
	procfs-non-linux.stderr.exp-with-readlinkat \
	procfs-non-linux.stderr.exp-without-readlinkat \
	pselect_alarm.stdout.exp pselect_alarm.stderr.exp pselect_alarm.vgtest \
	pselect_sigmask_null.vgtest \
	pselect_sigmask_null.stdout.exp pselect_sigmask_null.stderr.exp \
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
	pth_self_kill_9.stderr.exp pth_self_kill_9.vgtest \
	pth_self_kill_15_other.stderr.exp pth_self_kill_15_other.vgtest \
	pth_stackalign.stderr.exp \
	pth_stackalign.stdout.exp pth_stackalign.vgtest \
	pth_2sig.stderr.exp-linux pth_2sig.stderr.exp-solaris pth_2sig.vgtest \
	pth_term_signal.stderr.exp pth_term_signal.vgtest \
	rc_option_with_spaces.stderr.exp rc_option_with_spaces.vgtest \
	rcrl.stderr.exp rcrl.stdout.exp rcrl.vgtest \
	readline1.stderr.exp readline1.stdout.exp \
	readline1.vgtest \
	require-text-symbol-1.vgtest \
		require-text-symbol-1.stderr.exp \
	require-text-symbol-2.vgtest \
		require-text-symbol-2.stderr.exp-libcso1 \
		require-text-symbol-2.stderr.exp-libcso6 \
		require-text-symbol-2.stderr.exp-freebsd \
	res_search.stderr.exp res_search.stdout.exp res_search.vgtest \
	resolv.stderr.exp resolv.stdout.exp resolv.vgtest \
	rlimit_nofile.stderr.exp rlimit_nofile.stdout.exp rlimit_nofile.vgtest \
	rlimit64_nofile.stderr.exp rlimit64_nofile.stdout.exp rlimit64_nofile.vgtest \
	selfrun.stderr.exp selfrun.stdout.exp selfrun.vgtest \
	sem.stderr.exp sem.stdout.exp sem.vgtest \
	semlimit.stderr.exp semlimit.stdout.exp semlimit.vgtest \
	sha1_test.stderr.exp sha1_test.vgtest \
	shortpush.stderr.exp shortpush.vgtest \
	shorts.stderr.exp shorts.vgtest \
	sigstackgrowth.stdout.exp sigstackgrowth.stderr.exp sigstackgrowth.vgtest \
	sigsusp.stderr.exp sigsusp.vgtest \
	stackgrowth.stdout.exp stackgrowth.stderr.exp stackgrowth.vgtest \
	syscall-restart1.vgtest syscall-restart1.stdout.exp syscall-restart1.stderr.exp \
	syscall-restart2.vgtest syscall-restart2.stdout.exp syscall-restart2.stderr.exp \
	syslog.vgtest syslog.stderr.exp \
	system.stderr.exp system.vgtest \
	thread-exits.stderr.exp thread-exits.stdout.exp thread-exits.vgtest \
	threaded-fork.stderr.exp threaded-fork.stdout.exp threaded-fork.vgtest \
	threadederrno.stderr.exp threadederrno.stdout.exp \
	threadederrno.vgtest \
	timer_delete.vgtest timer_delete.stderr.exp \
	timestamp.stderr.exp timestamp.vgtest \
	tls.vgtest tls.stderr.exp tls.stdout.exp  \
	track-fds-exec-children.vgtest track-fds-exec-children.stderr.exp \
	unit_debuglog.stderr.exp unit_debuglog.vgtest \
	vgprintf.stderr.exp vgprintf.vgtest \
	vgprintf_nvalgrind.stderr.exp vgprintf_nvalgrind.vgtest \
	process_vm_readv_writev.stderr.exp process_vm_readv_writev.vgtest \
	sigprocmask.stderr.exp sigprocmask.vgtest \
	socket_close.stderr.exp socket_close.vgtest \
	socket_close_xml.stderr.exp-ppc64le \
	socket_close_xml.stderr.exp socket_close_xml.vgtest \
	file_dclose.stderr.exp file_dclose.vgtest \
	file_dclose_xml.stderr.exp file_dclose_xml.vgtest \
	file_dclose_xml.stderr.exp-nomain \
	file_dclose.supp file_dclose_sup.stderr.exp file_dclose_sup.vgtest \
	double_close_range.stderr.exp double_close_range.vgtest \
	log-track-fds.stderr.exp log-track-fds.vgtest \
	xml-track-fds.stderr.exp xml-track-fds.vgtest \
	fdbaduse.stderr.exp fdbaduse.vgtest \
	use_after_close.stderr.exp use_after_close.vgtest \
	track_new.stderr.exp track_new.stdout.exp \
	track_new.vgtest track_new.stderr.exp-illumos \
	track_yes.vgtest track_high.vgtest \
	track_yes.stderr.exp track_high.stderr.exp \
	track_bad.vgtest track_bad.stderr.exp

check_PROGRAMS = \
	args \
	async-sigs \
	bitfield1 \
	bug129866 bug234814 \
	bug290061 \
	bug492678 \
	closeall coolo_strlen \
	discard exec-sigmask execve faultstatus fcntl_setown \
	fdleak_cmsg fdleak_creat fdleak_doubleclose0 fdleak_dup fdleak_dup2 \
	fdleak_fcntl fdleak_ipv4 fdleak_open fdleak_pipe \
	fdleak_socketpair \
	floored fork fucomip \
	ioctl_moans \
	libvex_test \
	libvexmultiarch_test \
	manythreads \
	mmap_fcntl_bug \
	mmap_o_direct \
	munmap_exe map_unaligned map_unmap mq \
	nocwd \
	pending \
	procfs-cmdline-exe \
	pselect_alarm \
	pselect_sigmask_null \
	pth_atfork1 pth_blockedsig pth_cancel1 pth_cancel2 pth_cvsimple \
	pth_empty pth_exit pth_exit2 pth_mutexspeed pth_once pth_rwlock \
	pth_self_kill pth_stackalign pth_2sig pth_term_signal\
	rcrl readline1 \
	require-text-symbol \
	res_search resolv \
	rlimit_nofile selfrun sem semlimit sha1_test \
	shortpush shorts stackgrowth sigstackgrowth sigsusp \
	syscall-restart1 syscall-restart2 \
	syslog \
	system \
	thread-exits \
	threaded-fork \
	threadederrno \
	timestamp \
	tls \
	tls.so \
	tls2.so \
	track-fds-exec-children \
	unit_debuglog \
	valgrind_cpp_test \
	vgprintf \
	vgprintf_nvalgrind \
	coolo_sigaction \
	gxx304 \
	process_vm_readv_writev \
	sigprocmask \
	socket_close \
	file_dclose \
	fdbaduse \
        use_after_close \
	track_new \
	track_std \
	track_bad

if HAVE_STATIC_LIBC
if ! VGCONF_OS_IS_LINUX
   check_PROGRAMS += bug491394
endif
if VGCONF_OS_IS_LINUX
if VGCONF_ARCHS_INCLUDE_AMD64
   check_PROGRAMS += bug491394
endif
endif
endif

if HAVE_CLOSE_RANGE
   check_PROGRAMS += double_close_range
endif

if HAVE_NESTED_FUNCTIONS
   check_PROGRAMS += nestedfns
   nestedfns_LDFLAGS = @FLAG_NO_WARN_EXECSTACK@
endif

# This doesn't appear to be compilable on Darwin.
if ! VGCONF_OS_IS_DARWIN_OR_FREEBSD
   check_PROGRAMS += rlimit64_nofile
endif

if ! VGCONF_OS_IS_DARWIN
   check_PROGRAMS += ppoll_alarm
endif

# older (?) clang does not know -ansi
if HAVE_ANSI
   check_PROGRAMS += ansi
endif

if BUILD_IFUNC_TESTS
   check_PROGRAMS += ifunc
endif

if HAVE_TIMER_DELETE
   check_PROGRAMS += timer_delete
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# Extra stuff for C tests
ansi_CFLAGS		= $(AM_CFLAGS) -ansi
bug290061_CFLAGS	= -fPIC
bug290061_LDFLAGS	= @FLAG_PIE@
bug491394_LDADD		= -lc
bug491394_LDFLAGS	= -nostdlib -static
bug491394_CFLAGS	= ${AM_CFLAGS} -Os
execve_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
if VGCONF_OS_IS_SOLARIS
fcntl_setown_LDADD	= -lsocket -lnsl
fdleak_cmsg_CFLAGS	= $(AM_CFLAGS) -D_XOPEN_SOURCE=600
fdleak_cmsg_LDADD	= -lsocket -lnsl
fdleak_ipv4_LDADD	= -lsocket -lnsl
fdleak_creat_LDADD	= -lsocket -lnsl
fdleak_socketpair_LDADD	= -lsocket -lnsl
socket_close_LDADD	= -lsocket -lnsl
else
fcntl_setown_LDADD	= $(AL_LDFLAGS)
fdleak_cmsg_CFLAGS	= $(AM_CFLAGS)
fdleak_cmsg_LDADD	= $(AM_LDFLAGS)
fdleak_ipv4_LDADD	= $(AM_LDFLAGS)
fdleak_creat_LDADD	= $(AM_LDFLAGS)
fdleak_socketpair_LDADD	= $(AM_LDFLAGS)
socket_close_LDADD	= $(AM_LDFLAGS)
endif
floored_LDADD 		= -lm
ifunc_CFLAGS		= $(AM_CFLAGS) -Wno-unused-function
manythreads_LDADD	= -lpthread
if VGCONF_OS_IS_DARWIN
 nestedfns_CFLAGS	= $(AM_CFLAGS) -fnested-functions
else
 nestedfns_CFLAGS	= $(AM_CFLAGS)
 mq_LDADD		= -lrt
endif
libvex_test_CFLAGS      = $(AM_CFLAGS) @FLAG_FSANITIZE@
libvex_test_LDADD       = ../../VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a \
			   @LIB_UBSAN@
libvexmultiarch_test_CFLAGS= $(AM_CFLAGS) @FLAG_FSANITIZE@
if VGCONF_ARCHS_INCLUDE_RISCV64
# Disable RISC-V linker relaxation, it takes GNU ld 2.39 tens of minutes to sort
# it through on this large test.
libvexmultiarch_test_LDFLAGS = -Wl,--no-relax
endif
libvexmultiarch_test_LDADD = \
	../../VEX/libvexmultiarch-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a \
	../../VEX/libvex-@VGCONF_ARCH_PRI@-@VGCONF_OS@.a @LIB_UBSAN@
libvexmultiarch_test_SOURCES = libvex_test.c
ppoll_alarm_LDADD	= -lpthread
pselect_alarm_LDADD	= -lpthread
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
pth_rwlock_CFLAGS	= $(AM_CFLAGS)
pth_self_kill_LDADD	= -lpthread
pth_stackalign_LDADD	= -lpthread
pth_2sig_LDADD		= -lpthread
pth_term_signal_LDADD	= -lpthread
if VGCONF_OS_IS_FREEBSD
   res_search_LDADD      = -lpthread
   resolv_LDADD             = -lpthread
else
   res_search_LDADD      = -lresolv -lpthread
   resolv_LDADD             = -lresolv -lpthread
endif
resolv_CFLAGS		= $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
resolv_CFLAGS		+= -U_REENTRANT
endif
semlimit_LDADD		= -lpthread
sha1_test_CFLAGS	= $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
sha1_test_CFLAGS	+= -Du_int32_t=uint32_t
endif
sigsusp_LDADD		= -lpthread
thread_exits_LDADD	= -lpthread
threaded_fork_LDADD	= -lpthread
threadederrno_CFLAGS	= $(AM_CFLAGS)
threadederrno_LDADD	= -lpthread
if HAVE_TIMER_DELETE
   timer_delete_LDADD      = -lrt
endif
timestamp_CFLAGS	= ${AM_CFLAGS} @FLAG_W_NO_USE_AFTER_FREE@
tls_SOURCES		= tls.c tls2.c
tls_DEPENDENCIES	= tls.so tls2.so
tls_LDFLAGS		= -Wl,-rpath,$(abs_top_builddir)/none/tests
tls_LDADD		= tls.so tls2.so -lpthread
tls_so_SOURCES		= tls_so.c
tls_so_DEPENDENCIES	= tls2.so
if VGCONF_OS_IS_DARWIN
 tls_so_LDFLAGS		= -dynamic -dynamiclib -all_load -fpic
 tls_so_LDADD		= `pwd`/tls2.so
else
 tls_so_LDFLAGS		= -Wl,-rpath,$(abs_top_builddir)/none/tests -shared -fPIC
 tls_so_LDADD		= tls2.so
endif
tls_so_CFLAGS		= $(AM_CFLAGS) -fPIC
tls2_so_SOURCES		= tls2_so.c
if VGCONF_OS_IS_DARWIN
 tls2_so_LDFLAGS	= -dynamic -dynamiclib -all_load
else
 tls2_so_LDFLAGS	= -shared
endif
track_fds_exec_children_SOURCES = track-fds-exec-children.c

vgprintf_nvalgrind_SOURCES = vgprintf.c
vgprintf_nvalgrind_CFLAGS = ${AM_CFLAGS} -DNVALGRIND

valgrind_cpp_test_SOURCES = valgrind_cpp_test.cpp
valgrind_cpp_test_LDADD   = -lstdc++

# C++ tests
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp

