SUBDIRS      = ${VG_ARCH}     .
DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_allocs filter_leak_check_size \
		 filter_stderr filter_stderr_backtrace

noinst_HEADERS = scalar.h

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
	buflen_check.stderr.exp buflen_check.vgtest \
	clientperm.stderr.exp \
	clientperm.stdout.exp clientperm.vgtest \
	custom_alloc.stderr.exp custom_alloc.vgtest \
	describe-block.stderr.exp describe-block.vgtest \
	doublefree.stderr.exp doublefree.vgtest \
	error_counts.stderr.exp error_counts.stdout.exp error_counts.vgtest \
	errs1.stderr.exp errs1.vgtest \
	exitprog.stderr.exp exitprog.vgtest \
	execve.stderr.exp execve.vgtest \
	execve2.stderr.exp execve2.vgtest \
	fprw.stderr.exp fprw.vgtest \
	fwrite.stderr.exp fwrite.stdout.exp fwrite.vgtest \
	inits.stderr.exp inits.vgtest \
	inline.stderr.exp inline.stdout.exp inline.vgtest \
	leak-0.vgtest leak-0.stderr.exp \
	leak-cycle.vgtest leak-cycle.stderr.exp \
	leak-tree.vgtest leak-tree.stderr.exp \
	leak-regroot.vgtest leak-regroot.stderr.exp \
	leakotron.vgtest leakotron.stdout.exp leakotron.stderr.exp \
	malloc1.stderr.exp malloc1.vgtest \
	malloc2.stderr.exp malloc2.vgtest \
	malloc3.stderr.exp malloc3.stdout.exp malloc3.vgtest \
	manuel1.stderr.exp manuel1.stdout.exp manuel1.vgtest \
	manuel2.stderr.exp manuel2.stdout.exp manuel2.vgtest \
	manuel3.stderr.exp manuel3.vgtest \
	match-overrun.stderr.exp match-overrun.vgtest match-overrun.supp \
	memalign_test.stderr.exp memalign_test.vgtest \
	memalign2.stderr.exp memalign2.vgtest \
	memcmptest.stderr.exp memcmptest.stdout.exp memcmptest.vgtest \
	mempool.stderr.exp mempool.vgtest \
	mismatches.stderr.exp mismatches.vgtest \
	mmaptest.stderr.exp mmaptest.vgtest \
	nanoleak.stderr.exp nanoleak.vgtest \
	nanoleak_supp.stderr.exp nanoleak_supp.vgtest nanoleak.supp \
	new_nothrow.stderr.exp new_nothrow.vgtest \
	new_override.stderr.exp new_override.stdout.exp new_override.vgtest \
	null_socket.stderr.exp null_socket.vgtest \
	overlap.stderr.exp overlap.stdout.exp overlap.vgtest \
	pointer-trace.vgtest pointer-trace.stdout.exp pointer-trace.stderr.exp \
	post-syscall.stderr.exp post-syscall.stdout.exp post-syscall.vgtest \
	realloc1.stderr.exp realloc1.vgtest \
	realloc2.stderr.exp realloc2.vgtest \
	realloc3.stderr.exp realloc3.vgtest \
	scalar.stderr.exp scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_exit_group.stderr.exp scalar_exit_group.vgtest \
	scalar_supp.stderr.exp scalar_supp.vgtest scalar_supp.supp \
	scalar_vfork.stderr.exp scalar_vfork.vgtest \
	sigaltstack.stderr.exp sigaltstack.vgtest \
	signal2.stderr.exp signal2.stdout.exp signal2.vgtest \
	sigprocmask.stderr.exp sigprocmask.vgtest \
	str_tester.stderr.exp str_tester.vgtest \
	supp1.stderr.exp supp1.vgtest \
	supp2.stderr.exp supp2.vgtest \
	supp.supp \
	suppfree.stderr.exp suppfree.vgtest \
	toobig-allocs.stderr.exp toobig-allocs.vgtest \
	trivialleak.stderr.exp trivialleak.vgtest \
	weirdioctl.stderr.exp weirdioctl.stdout.exp weirdioctl.vgtest \
	metadata.stderr.exp metadata.stdout.exp metadata.vgtest \
	vgtest_ume.stderr.exp vgtest_ume.vgtest \
	writev.stderr.exp writev.stderr.exp2 writev.stderr.exp3 writev.vgtest \
	zeropage.stderr.exp zeropage.stderr.exp2 zeropage.vgtest

check_PROGRAMS = \
	addressable \
	badaddrvalue badfree badjump badjump2 \
	badloop badpoll badrw brk brk2 buflen_check \
	clientperm custom_alloc \
	describe-block \
	doublefree error_counts errs1 exitprog execve execve2 \
	fprw fwrite hello inits inline \
	leak-0 leak-cycle leak-tree leak-regroot leakotron \
	malloc1 malloc2 malloc3 manuel1 manuel2 manuel3 \
	match-overrun \
	memalign_test memalign2 memcmptest mempool mmaptest \
	nanoleak new_nothrow \
	null_socket overlap \
	pointer-trace \
	post-syscall \
	realloc1 realloc2 realloc3 \
	scalar scalar_exit_group scalar_fork scalar_supp scalar_vfork \
	sigaltstack signal2 sigprocmask \
	str_tester supp1 supp2 suppfree \
	trivialleak weirdioctl	\
	mismatches new_override metadata \
	vgtest_ume \
	writev zeropage


AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include -I@VEX_DIR@/pub
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 
AM_CXXFLAGS = $(AM_CFLAGS)

# C ones
addressable_SOURCES	= addressable.c
badaddrvalue_SOURCES 	= badaddrvalue.c
badfree_SOURCES 	= badfree.c
badjump_SOURCES 	= badjump.c
badjump2_SOURCES 	= badjump2.c
badloop_SOURCES 	= badloop.c
badpoll_SOURCES		= badpoll.c
badrw_SOURCES		= badrw.c
brk_SOURCES 		= brk.c
brk2_SOURCES 		= brk2.c
buflen_check_SOURCES	= buflen_check.c
clientperm_SOURCES 	= clientperm.c
custom_alloc_SOURCES 	= custom_alloc.c
describe_block_SOURCES	= describe-block.c
doublefree_SOURCES 	= doublefree.c
error_counts_SOURCES 	= error_counts.c
errs1_SOURCES 		= errs1.c
execve_SOURCES 		= execve.c
execve2_SOURCES 	= execve2.c
exitprog_SOURCES 	= exitprog.c
fprw_SOURCES 		= fprw.c
fwrite_SOURCES 		= fwrite.c
inits_SOURCES		= inits.c
inline_SOURCES 	        = inline.c
leak_0_SOURCES		= leak-0.c
leak_cycle_SOURCES	= leak-cycle.c
leak_tree_SOURCES	= leak-tree.c
leak_regroot_SOURCES	= leak-regroot.c
leakotron_SOURCES	= leakotron.c
malloc1_SOURCES 	= malloc1.c
malloc2_SOURCES 	= malloc2.c
malloc3_SOURCES 	= malloc3.c
manuel1_SOURCES 	= manuel1.c
manuel2_SOURCES 	= manuel2.c
manuel3_SOURCES 	= manuel3.c
match_overrun_SOURCES 	= match-overrun.c
mmaptest_SOURCES 	= mmaptest.c
memalign_test_SOURCES 	= memalign_test.c
memalign2_SOURCES 	= memalign2.c
memcmptest_SOURCES 	= memcmptest.c
memcmptest_CFLAGS	= $(AM_CFLAGS) -fno-builtin-memcmp
mempool_SOURCES 	= mempool.c
nanoleak_SOURCES 	= nanoleak.c
null_socket_SOURCES 	= null_socket.c
overlap_SOURCES 	= overlap.c
# Don't allow GCC to inline memcpy(), because then we can't intercept it
overlap_CFLAGS		= $(AM_CFLAGS) -fno-builtin-memcpy
pointer_trace_SOURCES	= pointer-trace.c
post_syscall_SOURCES	= post-syscall.c
realloc1_SOURCES 	= realloc1.c
realloc2_SOURCES 	= realloc2.c
realloc3_SOURCES 	= realloc3.c
scalar_SOURCES 		= scalar.c
scalar_exit_group_SOURCES 	= scalar_exit_group.c
scalar_fork_SOURCES 	= scalar_fork.c
scalar_supp_SOURCES 	= scalar_supp.c
scalar_vfork_SOURCES 	= scalar_vfork.c
signal2_SOURCES 	= signal2.c
sigprocmask_SOURCES 	= sigprocmask.c
supp1_SOURCES 		= supp.c
supp2_SOURCES 		= supp.c
suppfree_SOURCES 	= suppfree.c
sigaltstack_SOURCES 	= sigaltstack.c
trivialleak_SOURCES 	= trivialleak.c
weirdioctl_SOURCES 	= weirdioctl.c
metadata_SOURCES	= metadata.c
str_tester_SOURCES	= str_tester.c
str_tester_CFLAGS	= $(AM_CFLAGS) -Wno-shadow
writev_SOURCES		= writev.c
zeropage_SOURCES	= zeropage.c

# C++ ones
mismatches_SOURCES	= mismatches.cpp
new_nothrow_SOURCES 	= new_nothrow.cpp
new_override_SOURCES 	= new_override.cpp

# Valgrind unit self-tests
hello_SOURCES		= hello.c
## If we support PIE, build hello as a PIE, otherwise vgtest_ume cannot load
## (I'm not completely sure why, since stage2 can load both PIEs and non-PIEs,
## maybe it's because stage2 is loaded up high;  anyway, it doesn't really
## matter so long as vgtest_ume can load hello ok.  --njn)
if USE_PIE
hello_CFLAGS		= $(AM_CFLAGS) -fpie
hello_LDFLAGS		= -pie
else
hello_LDFLAGS		= -Wl,-defsym,kickstart_base=0x50000000 \
			  -Wl,-T,../../coregrind/${VG_ARCH}/stage2.lds
hello_DEPENDENCIES 	= ../../coregrind/${VG_ARCH}/stage2.lds
endif

vgtest_ume_SOURCES	= vgtest_ume.c
vgtest_ume_LDADD	= ../../coregrind/ume.o \
			  ../../coregrind/jmp_with_stack.o

