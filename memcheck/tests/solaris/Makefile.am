
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_ldynsym \
	filter_name_service_door \
	filter_scalar \
	filter_sendfilev \
	filter_stderr \
	filter_syscall_at

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	aligned_alloc.stderr.exp aligned_alloc.vgtest \
	aligned_alloc.stderr.exp-solaris \
	brk.stderr.exp brk.stdout.exp brk.vgtest \
	context_stack_die.stderr.exp context_stack_die.stdout.exp context_stack_die.vgtest \
	door_data.stderr.exp door_data.stdout.exp door_data.vgtest \
	door_kill.stderr.exp door_kill.stdout.exp door_kill.vgtest \
	execx.stderr.exp execx.stdout.exp execx.vgtest \
	getzoneoffset.stderr.exp getzoneoffset.vgtest \
	gethrtime.stderr.exp gethrtime.stdout.exp gethrtime.vgtest \
	gethrusec.stderr.exp gethrusec.stdout.exp gethrusec.vgtest \
	ioctl.stderr.exp ioctl.stdout.exp ioctl.vgtest \
	ldynsym.stderr.exp ldynsym.stdout.exp ldynsym.vgtest \
	lsframe1.stderr.exp lsframe1.stdout.exp lsframe1.vgtest \
	lsframe2.stderr.exp lsframe2.stdout.exp lsframe2.vgtest \
	mmapobj_bssonly.stderr.exp mmapobj_bssonly.stdout.exp mmapobj_bssonly.vgtest \
	name_service_door.stderr.exp-amd64 name_service_door.stderr.exp-x86 \
	name_service_door.stdout.exp name_service_door.vgtest \
	pkcs11.stderr.exp-illumos pkcs11.stderr.exp-solaris pkcs11.stdout.exp pkcs11.vgtest \
	scalar.stderr.exp scalar.stdout.exp scalar.vgtest \
	scalar_auditon_stat.stderr.exp scalar_auditon_stat.stdout.exp scalar_auditon_stat.vgtest \
	scalar_frealpathat.stderr.exp scalar_frealpathat.stdout.exp scalar_frealpathat.vgtest \
	scalar_getrandom.stderr.exp scalar_getrandom.stdout.exp scalar_getrandom.vgtest \
	scalar_ioctl.stderr.exp scalar_ioctl.stdout.exp scalar_ioctl.vgtest \
	scalar_lwp_kill.stderr.exp scalar_lwp_kill.stdout.exp scalar_lwp_kill.vgtest \
	scalar_lwp_name.stderr.exp scalar_lwp_name.stdout.exp scalar_lwp_name.vgtest \
	scalar_lwp_sigqueue.stderr.exp scalar_lwp_sigqueue.stdout.exp scalar_lwp_sigqueue.vgtest \
	scalar_lwp_sigqueue_pid.stderr.exp scalar_lwp_sigqueue_pid.vgtest \
	scalar_modctl_modnvl.stderr.exp scalar_modctl_modnvl.stdout.exp scalar_modctl_modnvl.vgtest \
	scalar_obsolete.stderr.exp scalar_obsolete.stdout.exp scalar_obsolete.vgtest \
	scalar_shm_new.stderr.exp scalar_shm_new.stdout.exp scalar_shm_new.vgtest \
	scalar_spawn.stderr.exp scalar_spawn.stdout.exp scalar_spawn.vgtest \
	scalar_system_stats.stderr.exp scalar_system_stats.stdout.exp scalar_system_stats.vgtest \
	scalar_tsol_clearance.stderr.exp scalar_tsol_clearance.vgtest \
	scalar_utimensat.stderr.exp scalar_utimensat.stdout.exp scalar_utimensat.vgtest \
	scalar_utimesys.stderr.exp scalar_utimesys.stdout.exp scalar_utimesys.vgtest \
	scalar_uuidsys.stderr.exp scalar_uuidsys.stdout.exp scalar_uuidsys.vgtest \
	scalar_zone_defunct.stderr.exp scalar_zone_defunct.stdout.exp scalar_zone_defunct.vgtest \
	sendfilev.stderr.exp sendfilev.stdout.exp sendfilev.vgtest \
	shmat.stderr.exp shmat.stdout.exp shmat.vgtest \
	spawn.stderr.exp spawn.stdout.exp spawn.vgtest \
	strlcpy.stderr.exp strlcpy.stdout.exp strlcpy.vgtest \
	supponlyobj.stderr.exp supponlyobj.supp supponlyobj.vgtest \
	syscall_at.stderr.exp syscall_at.stdout.exp syscall_at.vgtest \
	thr_daemon_exit_libc.stderr.exp thr_daemon_exit_libc.stdout.exp thr_daemon_exit_libc.vgtest \
	thr_daemon_exit_standalone.stderr.exp thr_daemon_exit_standalone.stdout.exp thr_daemon_exit_standalone.vgtest \
	memalign.vgtest memalign.stderr.exp

check_PROGRAMS = \
	aligned_alloc \
	brk \
	context_stack_die \
	door_data \
	door_kill \
	gethrtime \
	inlinfo \
	inlinfo_nested.so \
	ioctl \
	ldynsym \
	lsframe1 \
	lsframe2 \
	mmapobj_bssonly \
	mmapobj_bssonly.so \
	name_service_door \
	pkcs11 \
	scalar \
	scalar_ioctl \
	sendfilev \
	shmat \
	strlcpy \
	syscall_at \
	thr_daemon_exit_libc \
	thr_daemon_exit_standalone \
	memalign

if SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS
check_PROGRAMS += execx
endif

if SOLARIS_RESERVE_SYSSTAT_ADDR
check_PROGRAMS += gethrusec
endif

if SOLARIS_GETZONEOFFSET_FASTTRAP
check_PROGRAMS += getzoneoffset
endif

if SOLARIS_OLD_SYSCALLS
check_PROGRAMS += scalar_obsolete
endif

if SOLARIS_AUDITON_STAT
check_PROGRAMS += scalar_auditon_stat
endif

if SOLARIS_FREALPATHAT_SYSCALL
check_PROGRAMS += scalar_frealpathat
endif

if SOLARIS_GETRANDOM_SYSCALL
check_PROGRAMS += scalar_getrandom
endif

if SOLARIS_LWP_NAME_SYSCALL
check_PROGRAMS += scalar_lwp_name
endif

if SOLARIS_MODCTL_MODNVL
check_PROGRAMS += scalar_modctl_modnvl
endif

if SOLARIS_LWP_SIGQUEUE_SYSCALL
if SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID
check_PROGRAMS += scalar_lwp_sigqueue_pid
else
check_PROGRAMS += scalar_lwp_sigqueue
endif
else
check_PROGRAMS += scalar_lwp_kill
endif

if SOLARIS_SHM_NEW
check_PROGRAMS += scalar_shm_new
endif

if SOLARIS_SPAWN_SYSCALL
check_PROGRAMS += scalar_spawn spawn
endif

if SOLARIS_SYSTEM_STATS_SYSCALL
check_PROGRAMS += scalar_system_stats
endif

if SOLARIS_TSOL_CLEARANCE
check_PROGRAMS += scalar_tsol_clearance
scalar_tsol_clearance_LDADD = -ltsol
endif

if SOLARIS_UTIMENSAT_SYSCALL
check_PROGRAMS += scalar_utimensat
endif

if SOLARIS_UTIMESYS_SYSCALL
check_PROGRAMS += scalar_utimesys
endif

if SOLARIS_UUIDSYS_SYSCALL
check_PROGRAMS += scalar_uuidsys
endif

if SOLARIS_ZONE_DEFUNCT
check_PROGRAMS += scalar_zone_defunct
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

door_kill_LDADD = -lpthread
ioctl_LDADD = -lsocket
ldynsym_LDFLAGS = -Wl,--strip-all
pkcs11_LDADD = -lpkcs11
sendfilev_LDADD = -lsendfile

inlinfo_SOURCES = inlinfo.c
inlinfo_DEPENDENCIES = inlinfo_nested.so
inlinfo_LDFLAGS = -Wl,-rpath,$(top_builddir)/memcheck/tests/solaris
inlinfo_LDADD = inlinfo_nested.so
inlinfo_nested_so_SOURCES = inlinfo_nested.c
inlinfo_nested_so_CFLAGS = $(AM_CFLAGS) -fPIC @FLAG_W_NO_UNINITIALIZED@
inlinfo_nested_so_LDFLAGS = -Wl,-rpath,$(top_builddir)/memcheck/tests/solaris -shared -fPIC

mmapobj_bssonly_SOURCES = mmapobj_bssonly.c
mmapobj_bssonly_DEPENDENCIES = mmapobj_bssonly.so
mmapobj_bssonly_LDFLAGS = -Wl,-rpath,$(top_builddir)/memcheck/tests/solaris
mmapobj_bssonly_LDADD = mmapobj_bssonly.so
mmapobj_bssonly_so_SOURCES = mmapobj_bssonly_lib.c
mmapobj_bssonly_so_CFLAGS = $(AM_CFLAGS) -fPIC
mmapobj_bssonly_so_LDFLAGS = -nostartfiles -nodefaultlibs -shared

thr_daemon_exit_standalone_LDFLAGS = -nostartfiles -nodefaultlibs
