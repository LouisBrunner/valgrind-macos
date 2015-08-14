
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_coredump_many_threads_post \
	filter_coredump_many_threads_stderr \
	filter_stderr

EXTRA_DIST = \
	block_all_signals.stderr.exp block_all_signals.stdout.exp block_all_signals.vgtest \
	blockfault.stderr.exp blockfault.vgtest \
	context_link.stderr.exp context_link.stdout.exp context_link.vgtest \
	context_link2.stderr.exp context_link2.stdout.exp context_link2.vgtest \
	context_null.stderr.exp context_null.stdout.exp context_null.vgtest \
	context_stack.stderr.exp context_stack.stdout.exp context_stack.vgtest \
	coredump_many_threads.post.exp coredump_many_threads.stderr.exp coredump_many_threads.vgtest \
	mmap_noreserve.stderr.exp mmap_noreserve.stdout.exp mmap_noreserve.vgtest \
	proc_aout.stderr.exp proc_aout.stdout.exp proc_aout.vgtest \
	proc_auxv.stderr.exp proc_auxv.stdout.exp proc_auxv.vgtest \
	proc_auxv_multiple.stderr.exp proc_auxv_multiple.stdout.exp proc_auxv_multiple.vgtest \
	proc-cmdline-exe.stderr.exp-with-cmdline proc-cmdline-exe.stderr.exp-without-cmdline \
	    proc-cmdline-exe.vgtest \
	proc_psinfo.stderr.exp proc_psinfo.stdout.exp proc_psinfo.vgtest \
	posix_spawn.stderr.exp posix_spawn.stdout.exp posix_spawn.vgtest \
	pthread-stack.stderr.exp pthread-stack.vgtest \
	reserve_sysstat_addr.map reserve_sysstat_addr.stderr.exp reserve_sysstat_addr.vgtest \
	reserve_sysstat_zone_addr.map reserve_sysstat_zone_addr.stderr.exp reserve_sysstat_zone_addr.vgtest \
	resolv.stdout.exp resolv.stderr.exp resolv.vgtest \
	sigresend.stderr.exp sigresend.stdout.exp sigresend.vgtest \
	stack-overflow.stderr.exp stack-overflow.vgtest \
	stack_prot.stderr.exp stack_prot.stdout.exp stack_prot.vgtest \
	threads_exitall.stderr.exp threads_exitall.stdout.exp threads_exitall.vgtest

check_PROGRAMS = \
	block_all_signals \
	blockfault \
	context_link \
	context_link2 \
	context_null \
	context_stack \
	coredump_many_segments \
	coredump_many_threads \
	mmap_noreserve \
	proc_aout \
	proc_auxv \
	proc_auxv_multiple \
	proc_psinfo \
	posix_spawn \
	pthread-stack \
	resolv \
	sigresend \
	stack-overflow \
	stack_prot \
	threads_exitall

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

pthread_stack_LDADD = -lpthread
resolv_LDADD = -lresolv
stack_prot_LDFLAGS = -Wl,-M,/usr/lib/ld/map.noexstk
threads_exitall_LDADD = -lpthread

if SOLARIS_RESERVE_SYSSTAT_ADDR
check_PROGRAMS += reserve_sysstat_addr
reserve_sysstat_addr_LDFLAGS = -Wl,-M,reserve_sysstat_addr.map
endif

if SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR
check_PROGRAMS += reserve_sysstat_zone_addr
reserve_sysstat_zone_addr_LDFLAGS = -Wl,-M,reserve_sysstat_zone_addr.map
endif
