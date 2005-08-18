SUBDIRS      = ${VG_ARCH}     .
DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_allocs filter_leak_check_size \
		 filter_stderr filter_stderr_backtrace filter_xml

EXTRA_DIST = $(noinst_SCRIPTS) \
	addressable.stderr.exp addressable.stdout.exp addressable.vgtest \
	badaddrvalue.stderr.exp \
	badaddrvalue.stdout.exp badaddrvalue.vgtest \
	badfree-2trace.stderr.exp badfree-2trace.vgtest \
	badfree.stderr.exp badfree.vgtest \
	badjump.stderr.exp badjump.vgtest \
	badjump2.stderr.exp badjump2.vgtest \
	badloop.stderr.exp badloop.vgtest \
	badpoll.stderr.exp badpoll.vgtest \
	badrw.stderr.exp badrw.vgtest \
	brk.stderr.exp brk.vgtest \
	brk2.stderr.exp brk2.vgtest \
	buflen_check.stderr.exp buflen_check.stderr.exp2 buflen_check.vgtest \
	clientperm.stderr.exp \
	clientperm.stdout.exp clientperm.vgtest \
	custom_alloc.stderr.exp custom_alloc.vgtest \
	describe-block.stderr.exp describe-block.vgtest \
	doublefree.stderr.exp doublefree.vgtest \
	erringfds.stderr.exp erringfds.stdout.exp erringfds.vgtest \
	error_counts.stderr.exp error_counts.stdout.exp error_counts.vgtest \
	errs1.stderr.exp errs1.vgtest \
	exitprog.stderr.exp exitprog.vgtest \
	execve.stderr.exp execve.stderr.exp2 execve.vgtest \
	execve2.stderr.exp execve2.stderr.exp2 execve2.vgtest \
	fprw.stderr.exp fprw.vgtest \
	fwrite.stderr.exp fwrite.stderr.exp2 fwrite.stdout.exp fwrite.vgtest \
	inits.stderr.exp inits.vgtest \
	inline.stderr.exp inline.stdout.exp inline.vgtest \
	leak-0.vgtest leak-0.stderr.exp \
	leak-cycle.vgtest leak-cycle.stderr.exp leak-cycle.stderr.exp64 \
	leak-tree.vgtest leak-tree.stderr.exp \
	leak-tree.stderr.exp2 leak-tree.stderr.exp64 \
	leak-regroot.vgtest leak-regroot.stderr.exp \
	leakotron.vgtest leakotron.stdout.exp leakotron.stderr.exp \
	malloc_usable.stderr.exp malloc_usable.vgtest \
	malloc1.stderr.exp malloc1.vgtest \
	malloc2.stderr.exp malloc2.vgtest \
	malloc3.stderr.exp malloc3.stdout.exp malloc3.vgtest \
	manuel1.stderr.exp manuel1.stdout.exp manuel1.vgtest \
	manuel2.stderr.exp manuel2.stderr.exp64 manuel2.stdout.exp manuel2.vgtest \
	manuel3.stderr.exp manuel3.vgtest \
	match-overrun.stderr.exp match-overrun.vgtest match-overrun.supp \
	memalign_test.stderr.exp memalign_test.vgtest \
	memalign2.stderr.exp memalign2.vgtest \
	memcmptest.stderr.exp memcmptest.stderr.exp2 \
	memcmptest.stdout.exp memcmptest.vgtest \
	mempool.stderr.exp mempool.stderr.exp64 mempool.vgtest \
	mismatches.stderr.exp mismatches.stderr.exp64 mismatches.vgtest \
	mmaptest.stderr.exp mmaptest.vgtest \
	nanoleak.stderr.exp nanoleak.vgtest \
	nanoleak_supp.stderr.exp nanoleak_supp.vgtest nanoleak.supp \
	new_nothrow.stderr.exp new_nothrow.vgtest \
	new_override.stderr.exp new_override.stdout.exp new_override.vgtest \
	null_socket.stderr.exp null_socket.vgtest \
	overlap.stderr.exp overlap.stdout.exp overlap.vgtest \
	oset_test.stderr.exp oset_test.stdout.exp oset_test.vgtest \
	partiallydefinedeq.vgtest partiallydefinedeq.stderr.exp \
	partiallydefinedeq.stdout.exp \
	pointer-trace.vgtest \
	pointer-trace.stderr.exp pointer-trace.stderr.exp64 \
	post-syscall.stderr.exp post-syscall.stdout.exp post-syscall.vgtest \
	realloc1.stderr.exp realloc1.vgtest \
	realloc2.stderr.exp realloc2.vgtest \
	realloc3.stderr.exp realloc3.vgtest \
	sigaltstack.stderr.exp sigaltstack.vgtest \
	sigkill.stderr.exp sigkill.stderr.exp2 sigkill.vgtest \
	signal2.stderr.exp signal2.stdout.exp signal2.vgtest \
	sigprocmask.stderr.exp sigprocmask.vgtest \
	stack_changes.stderr.exp stack_changes.stdout.exp stack_changes.vgtest \
	strchr.stderr.exp strchr.vgtest \
	str_tester.stderr.exp str_tester.vgtest \
	supp_unknown.stderr.exp supp_unknown.vgtest supp_unknown.supp \
	supp1.stderr.exp supp1.vgtest \
	supp2.stderr.exp supp2.vgtest \
	supp.supp \
	suppfree.stderr.exp suppfree.vgtest \
	toobig-allocs.stderr.exp toobig-allocs.vgtest \
	trivialleak.stderr.exp trivialleak.vgtest \
	weirdioctl.stderr.exp weirdioctl.stdout.exp weirdioctl.vgtest \
	metadata.stderr.exp metadata.stdout.exp metadata.vgtest-HIDING \
	vgtest_ume.stderr.exp vgtest_ume.vgtest \
	writev.stderr.exp writev.stderr.exp2 writev.stderr.exp3 writev.vgtest \
	xml1.stderr.exp xml1.stderr.exp2 xml1.stderr.exp64 xml1.stdout.exp \
	xml1.vgtest \
	zeropage.stderr.exp zeropage.stderr.exp2 zeropage.stdout.exp \
	zeropage.vgtest

check_PROGRAMS = \
	addressable \
	badaddrvalue badfree badjump badjump2 \
	badloop badpoll badrw brk brk2 buflen_check \
	clientperm custom_alloc \
	describe-block \
	doublefree error_counts errs1 exitprog execve execve2 erringfds \
	fprw fwrite hello inits inline \
	leak-0 leak-cycle leak-tree leak-regroot leakotron \
	malloc_usable malloc1 malloc2 malloc3 manuel1 manuel2 manuel3 \
	match-overrun \
	memalign_test memalign2 memcmptest mempool mmaptest \
	nanoleak new_nothrow \
	null_socket oset_test overlap \
	partiallydefinedeq \
	pointer-trace \
	post-syscall \
	realloc1 realloc2 realloc3 \
	sigaltstack signal2 sigprocmask sigkill \
	stack_changes strchr str_tester supp_unknown supp1 supp2 suppfree \
	trivialleak weirdioctl	\
	mismatches new_override metadata \
	vgtest_ume xml1 \
	writev zeropage


AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/coregrind -I$(top_builddir)/include -I@VEX_DIR@/pub
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 
AM_CXXFLAGS = $(AM_CFLAGS)

# Extra stuff for C tests
memcmptest_CFLAGS	= $(AM_CFLAGS) -fno-builtin-memcmp
oset_test_CFLAGS	= -DVGA_$(VG_ARCH) -DVGO_$(VG_OS) -DVGP_$(VG_PLATFORM)
# Don't allow GCC to inline memcpy(), because then we can't intercept it
overlap_CFLAGS		= $(AM_CFLAGS) -fno-builtin-memcpy
str_tester_CFLAGS	= $(AM_CFLAGS) -Wno-shadow
supp_unknown_SOURCES	= badjump.c
supp1_SOURCES		= supp.c
supp2_SOURCES		= supp.c

# C++ tests
mismatches_SOURCES	= mismatches.cpp
new_nothrow_SOURCES 	= new_nothrow.cpp
new_override_SOURCES 	= new_override.cpp

# Valgrind unit self-tests
## If we support PIE, build hello as a PIE, otherwise vgtest_ume cannot load
## (I'm not completely sure why, since stage2 can load both PIEs and non-PIEs,
## maybe it's because stage2 is loaded up high;  anyway, it doesn't really
## matter so long as vgtest_ume can load hello ok.  --njn)
if USE_PIE
hello_CFLAGS		= $(AM_CFLAGS) -fpie
hello_LDFLAGS		= -pie
else
hello_LDFLAGS		= -Wl,-defsym,kickstart_base=0x50000000 \
			  -Wl,-T,../../coregrind/stage2.lds
hello_DEPENDENCIES 	= ../../coregrind/stage2.lds
endif

vgtest_ume_CFLAGS	= -DVGA_$(VG_ARCH) -DVGO_$(VG_OS)
vgtest_ume_LDADD	= ../../coregrind/m_ume.o

