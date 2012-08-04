
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
if VGCONF_ARCHS_INCLUDE_S390X
SUBDIRS += s390x
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
if VGCONF_PLATFORMS_INCLUDE_AMD64_LINUX
SUBDIRS += amd64-linux
endif

DIST_SUBDIRS = x86 amd64 ppc32 ppc64 s390x linux darwin x86-linux amd64-linux .

dist_noinst_SCRIPTS = \
	filter_addressable \
	filter_allocs \
	filter_leak_cases_possible \
	filter_stderr filter_xml \
	filter_varinfo3 \
	filter_memcheck

noinst_HEADERS = leak.h

EXTRA_DIST = \
	accounting.stderr.exp accounting.vgtest \
	addressable.stderr.exp addressable.stdout.exp addressable.vgtest \
	atomic_incs.stderr.exp atomic_incs.vgtest \
	atomic_incs.stdout.exp-32bit atomic_incs.stdout.exp-64bit \
	badaddrvalue.stderr.exp \
	badaddrvalue.stdout.exp badaddrvalue.vgtest \
	badfree-2trace.stderr.exp badfree-2trace.vgtest \
	badfree.stderr.exp badfree.vgtest \
	badfree3.stderr.exp badfree3.vgtest \
	badjump.stderr.exp badjump.vgtest \
	badjump2.stderr.exp badjump2.vgtest \
	badjump.stderr.exp-kfail \
	badjump2.stderr.exp-kfail \
	badloop.stderr.exp badloop.vgtest \
	badpoll.stderr.exp badpoll.vgtest \
	badrw.stderr.exp badrw.vgtest badrw.stderr.exp-s390x-mvc \
	big_blocks_freed_list.stderr.exp big_blocks_freed_list.vgtest \
	brk2.stderr.exp brk2.vgtest \
	buflen_check.stderr.exp buflen_check.vgtest buflen_check.stderr.exp-kfail \
	bug287260.stderr.exp bug287260.vgtest \
	calloc-overflow.stderr.exp calloc-overflow.vgtest\
	clientperm.stderr.exp \
	clientperm.stdout.exp clientperm.vgtest \
	clireq_nofill.stderr.exp \
	clireq_nofill.stdout.exp clireq_nofill.vgtest \
	clo_redzone_default.vgtest clo_redzone_128.vgtest \
	clo_redzone_default.stderr.exp clo_redzone_128.stderr.exp \
	custom_alloc.stderr.exp custom_alloc.vgtest custom_alloc.stderr.exp-s390x-mvc \
	custom-overlap.stderr.exp custom-overlap.vgtest \
	deep-backtrace.vgtest deep-backtrace.stderr.exp \
	deep_templates.vgtest \
	deep_templates.stdout.exp deep_templates.stderr.exp \
	describe-block.stderr.exp describe-block.vgtest \
	doublefree.stderr.exp doublefree.vgtest \
	dw4.vgtest dw4.stderr.exp dw4.stdout.exp \
	err_disable1.vgtest err_disable1.stderr.exp \
	err_disable2.vgtest err_disable2.stderr.exp \
	err_disable3.vgtest err_disable3.stderr.exp \
	err_disable4.vgtest err_disable4.stderr.exp \
	erringfds.stderr.exp erringfds.stdout.exp erringfds.vgtest \
	error_counts.stderr.exp error_counts.vgtest \
	errs1.stderr.exp errs1.vgtest \
	exitprog.stderr.exp exitprog.vgtest \
	execve1.stderr.exp execve1.vgtest execve1.stderr.exp-kfail \
	execve2.stderr.exp execve2.vgtest execve2.stderr.exp-kfail \
	file_locking.stderr.exp file_locking.vgtest \
	fprw.stderr.exp fprw.vgtest \
	fwrite.stderr.exp fwrite.vgtest fwrite.stderr.exp-kfail \
	holey_buffer_too_small.vgtest holey_buffer_too_small.stdout.exp \
	holey_buffer_too_small.stderr.exp \
	inits.stderr.exp inits.vgtest \
	inline.stderr.exp inline.stdout.exp inline.vgtest \
	leak-0.vgtest leak-0.stderr.exp \
	leak-cases-full.vgtest leak-cases-full.stderr.exp \
	leak-cases-possible.vgtest leak-cases-possible.stderr.exp \
	leak-cases-summary.vgtest leak-cases-summary.stderr.exp \
	leak-cycle.vgtest leak-cycle.stderr.exp \
	leak-delta.vgtest leak-delta.stderr.exp \
	leak-pool-0.vgtest leak-pool-0.stderr.exp \
	leak-pool-1.vgtest leak-pool-1.stderr.exp \
	leak-pool-2.vgtest leak-pool-2.stderr.exp \
	leak-pool-3.vgtest leak-pool-3.stderr.exp \
	leak-pool-4.vgtest leak-pool-4.stderr.exp \
	leak-pool-5.vgtest leak-pool-5.stderr.exp \
	leak-tree.vgtest leak-tree.stderr.exp \
	long_namespace_xml.vgtest long_namespace_xml.stdout.exp \
	long_namespace_xml.stderr.exp \
	long-supps.vgtest long-supps.stderr.exp long-supps.supp \
	mallinfo.stderr.exp mallinfo.vgtest \
	malloc_free_fill.vgtest \
	malloc_free_fill.stderr.exp \
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
	mempool.stderr.exp mempool.vgtest \
	mempool2.stderr.exp mempool2.vgtest \
	metadata.stderr.exp metadata.stdout.exp metadata.vgtest \
	mismatches.stderr.exp mismatches.vgtest \
	mmaptest.stderr.exp mmaptest.vgtest \
	nanoleak_supp.stderr.exp nanoleak_supp.vgtest nanoleak.supp \
	nanoleak2.stderr.exp nanoleak2.vgtest \
	new_nothrow.stderr.exp new_nothrow.vgtest \
	new_override.stderr.exp new_override.stdout.exp new_override.vgtest \
	noisy_child.vgtest noisy_child.stderr.exp noisy_child.stdout.exp \
	null_socket.stderr.exp null_socket.vgtest \
	origin1-yes.vgtest origin1-yes.stdout.exp origin1-yes.stderr.exp \
	origin2-not-quite.vgtest origin2-not-quite.stdout.exp \
	origin2-not-quite.stderr.exp \
	origin3-no.vgtest origin3-no.stdout.exp \
	origin3-no.stderr.exp \
	origin4-many.vgtest origin4-many.stdout.exp \
	origin4-many.stderr.exp \
	origin5-bz2.vgtest origin5-bz2.stdout.exp \
	origin5-bz2.stderr.exp-glibc25-x86 \
	origin5-bz2.stderr.exp-glibc25-amd64 \
	origin5-bz2.stderr.exp-glibc27-ppc64 \
	origin5-bz2.stderr.exp-glibc212-s390x \
	origin5-bz2.stderr.exp-glibc234-s390x \
	origin6-fp.vgtest origin6-fp.stdout.exp \
	origin6-fp.stderr.exp-glibc25-amd64 \
	origin6-fp.stderr.exp-glibc27-ppc64 \
	overlap.stderr.exp overlap.stdout.exp overlap.vgtest \
	partiallydefinedeq.vgtest partiallydefinedeq.stderr.exp \
	partiallydefinedeq.stderr.exp4 \
	partiallydefinedeq.stderr.exp3 \
	partiallydefinedeq.stderr.exp2 \
	partiallydefinedeq.stdout.exp \
	partial_load_ok.vgtest partial_load_ok.stderr.exp partial_load_ok.stderr.exp64 \
	partial_load_dflt.vgtest partial_load_dflt.stderr.exp partial_load_dflt.stderr.exp64 \
	partial_load_dflt.stderr.expr-s390x-mvc \
	pdb-realloc.stderr.exp pdb-realloc.vgtest \
	pdb-realloc2.stderr.exp pdb-realloc2.stdout.exp pdb-realloc2.vgtest \
	pipe.stderr.exp pipe.vgtest \
	pointer-trace.vgtest \
	pointer-trace.stderr.exp \
	post-syscall.stderr.exp post-syscall.vgtest \
	realloc1.stderr.exp realloc1.vgtest \
	realloc2.stderr.exp realloc2.vgtest \
	realloc3.stderr.exp realloc3.vgtest \
	sbfragment.stdout.exp sbfragment.stderr.exp sbfragment.vgtest \
	sh-mem.stderr.exp sh-mem.vgtest \
	sh-mem-random.stderr.exp sh-mem-random.stdout.exp64 \
	sh-mem-random.stdout.exp sh-mem-random.vgtest \
	sigaltstack.stderr.exp sigaltstack.vgtest \
	sigkill.stderr.exp sigkill.stderr.exp-darwin sigkill.stderr.exp-mips32 \
	sigkill.vgtest \
	signal2.stderr.exp signal2.stdout.exp signal2.vgtest \
	sigprocmask.stderr.exp sigprocmask.stderr.exp2 sigprocmask.vgtest \
	static_malloc.stderr.exp static_malloc.vgtest \
	strchr.stderr.exp strchr.stderr.exp2 strchr.stderr.exp-darwin \
	    strchr.stderr.exp3 strchr.vgtest \
	str_tester.stderr.exp str_tester.vgtest \
	supp-dir.vgtest supp-dir.stderr.exp \
	supp_unknown.stderr.exp supp_unknown.vgtest supp_unknown.supp \
	supp_unknown.stderr.exp-kfail \
	supp1.stderr.exp supp1.vgtest \
	supp2.stderr.exp supp2.vgtest \
	supp.supp \
	suppfree.stderr.exp suppfree.supp suppfree.vgtest \
	test-plo-no.vgtest test-plo-no.stdout.exp \
	    test-plo-no.stderr.exp-le64 test-plo-no.stderr.exp-le32 \
	test-plo-yes.vgtest test-plo-yes.stdout.exp \
	    test-plo-yes.stderr.exp-le64 test-plo-yes.stderr.exp-le32 \
	    test-plo-no.stderr.exp-s390x-mvc \
	trivialleak.stderr.exp trivialleak.vgtest trivialleak.stderr.exp2 \
	unit_libcbase.stderr.exp unit_libcbase.vgtest \
	unit_oset.stderr.exp unit_oset.stdout.exp unit_oset.vgtest \
	varinfo1.vgtest varinfo1.stdout.exp varinfo1.stderr.exp varinfo1.stderr.exp-ppc64 \
	varinfo2.vgtest varinfo2.stdout.exp varinfo2.stderr.exp varinfo2.stderr.exp-ppc64 \
	varinfo3.vgtest varinfo3.stdout.exp varinfo3.stderr.exp varinfo3.stderr.exp-ppc64 \
	varinfo4.vgtest varinfo4.stdout.exp varinfo4.stderr.exp varinfo4.stderr.exp-ppc64 \
	varinfo5.vgtest varinfo5.stdout.exp varinfo5.stderr.exp varinfo5.stderr.exp-ppc64 \
	varinfo6.vgtest varinfo6.stdout.exp varinfo6.stderr.exp varinfo6.stderr.exp-ppc64 \
	vcpu_bz2.stdout.exp vcpu_bz2.stderr.exp vcpu_bz2.vgtest \
	vcpu_fbench.stdout.exp vcpu_fbench.stderr.exp vcpu_fbench.vgtest \
	vcpu_fnfns.stdout.exp vcpu_fnfns.stdout.exp-glibc28-amd64 \
	vcpu_fnfns.stdout.exp-darwin vcpu_fnfns.stderr.exp vcpu_fnfns.vgtest \
	wrap1.vgtest wrap1.stdout.exp wrap1.stderr.exp \
	wrap2.vgtest wrap2.stdout.exp wrap2.stderr.exp \
	wrap3.vgtest wrap3.stdout.exp wrap3.stderr.exp \
	wrap4.vgtest wrap4.stdout.exp wrap4.stderr.exp \
	wrap5.vgtest wrap5.stdout.exp wrap5.stderr.exp \
	wrap6.vgtest wrap6.stdout.exp wrap6.stderr.exp \
	wrap7.vgtest wrap7.stdout.exp wrap7.stderr.exp \
	wrap8.vgtest wrap8.stdout.exp wrap8.stderr.exp \
	wrap8.stdout.exp2 wrap8.stderr.exp2 \
	writev1.stderr.exp writev1.vgtest \
	xml1.stderr.exp xml1.stdout.exp xml1.vgtest xml1.stderr.exp-s390x-mvc

check_PROGRAMS = \
	accounting \
	addressable \
	atomic_incs \
	badaddrvalue badfree badjump badjump2 \
	badloop \
	badpoll \
	badrw \
	big_blocks_freed_list \
	brk2 \
	buflen_check \
	bug287260 \
	calloc-overflow \
	clientperm \
	clireq_nofill \
	clo_redzone \
	custom_alloc \
	custom-overlap \
	deep-backtrace \
	deep_templates \
	describe-block \
	doublefree error_counts errs1 exitprog execve1 execve2 erringfds \
	err_disable1 err_disable2 err_disable3 err_disable4 \
	file_locking \
	fprw fwrite inits inline \
	holey_buffer_too_small \
	leak-0 \
	leak-cases \
	leak-cycle \
	leak-delta \
	leak-pool \
	leak-tree \
	long_namespace_xml \
	long-supps \
	mallinfo \
	malloc_free_fill \
	malloc_usable malloc1 malloc2 malloc3 manuel1 manuel2 manuel3 \
	match-overrun \
	memalign_test memalign2 memcmptest mempool mempool2 mmaptest \
	mismatches new_override metadata \
	nanoleak_supp nanoleak2 new_nothrow \
	noisy_child \
	null_socket \
	origin1-yes origin2-not-quite origin3-no \
	origin4-many origin5-bz2 origin6-fp \
	overlap \
	partiallydefinedeq \
	partial_load pdb-realloc pdb-realloc2 \
	pipe pointer-trace \
	post-syscall \
	realloc1 realloc2 realloc3 \
	sbfragment \
	sh-mem sh-mem-random \
	sigaltstack signal2 sigprocmask static_malloc sigkill \
	strchr \
	str_tester \
	supp_unknown supp1 supp2 suppfree \
	test-plo \
	trivialleak \
	unit_libcbase unit_oset \
	varinfo1 varinfo2 varinfo3 varinfo4 \
	varinfo5 varinfo5so.so varinfo6 \
	vcpu_fbench vcpu_fnfns \
	xml1 \
	wrap1 wrap2 wrap3 wrap4 wrap5 wrap6 wrap7 wrap7so.so wrap8 \
	writev1

if DWARF4
check_PROGRAMS += dw4
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

if VGCONF_PLATFORMS_INCLUDE_ARM_LINUX
#AM_CFLAGS   += -mfloat-abi=softfp
#AM_CXXFLAGS += -mfloat-abi=softfp
endif

if VGCONF_OS_IS_DARWIN
atomic_incs_CFLAGS = $(AM_CFLAGS) -mdynamic-no-pic
else
atomic_incs_CFLAGS = $(AM_CFLAGS)
endif

deep_templates_SOURCES	= deep_templates.cpp
deep_templates_CXXFLAGS	= $(AM_CFLAGS) -O -gstabs

dw4_CFLAGS		= $(AM_CFLAGS) -gdwarf-4 -fdebug-types-section

err_disable3_LDADD 	= -lpthread
err_disable4_LDADD 	= -lpthread

error_counts_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

execve1_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

execve2_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

inits_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

long_namespace_xml_SOURCES = long_namespace_xml.cpp

manuel1_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

memcmptest_CFLAGS	= $(AM_CFLAGS) -fno-builtin-memcmp

mismatches_SOURCES	= mismatches.cpp

new_nothrow_SOURCES 	= new_nothrow.cpp
new_override_SOURCES 	= new_override.cpp

origin2_not_quite_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

origin3_no_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

# This requires optimisation in order to get just one resulting error.
origin4_many_CFLAGS	= $(AM_CFLAGS) -O

# Apply -O so as to run in reasonable time.
origin5_bz2_CFLAGS	= $(AM_CFLAGS) -O -Wno-inline
origin6_fp_CFLAGS	= $(AM_CFLAGS) -O

# Don't allow GCC to inline memcpy() and strcpy(),
# because then we can't intercept it
overlap_CFLAGS		= $(AM_CFLAGS) -fno-builtin-memcpy -fno-builtin-strcpy

str_tester_CFLAGS	= $(AM_CFLAGS) -Wno-shadow

supp_unknown_SOURCES	= badjump.c
supp1_SOURCES		= supp.c
supp2_SOURCES		= supp.c

vcpu_bz2_CFLAGS		= $(AM_CFLAGS) -O2
vcpu_fbench_CFLAGS	= $(AM_CFLAGS) -O2
vcpu_fnfns_CFLAGS	= $(AM_CFLAGS) -O2
vcpu_fnfns_LDADD	= -lm
wrap6_CFLAGS		= $(AM_CFLAGS) -O2

# To make it a bit more realistic, have some optimisation enabled
# for the varinfo tests.  We still expect sane results.
varinfo1_CFLAGS		= $(AM_CFLAGS) -O
varinfo2_CFLAGS		= $(AM_CFLAGS) -O -Wno-shadow
varinfo3_CFLAGS		= $(AM_CFLAGS) -O
varinfo4_CFLAGS		= $(AM_CFLAGS) -O
varinfo5_CFLAGS		= $(AM_CFLAGS) -O
varinfo6_CFLAGS		= $(AM_CFLAGS) -O

# Build shared object for varinfo5
varinfo5_SOURCES        = varinfo5.c
varinfo5_DEPENDENCIES   = varinfo5so.so
if VGCONF_OS_IS_DARWIN
 varinfo5_LDADD         = `pwd`/varinfo5so.so
 varinfo5_LDFLAGS       = $(AM_FLAG_M3264_PRI)
else
 varinfo5_LDADD         = varinfo5so.so
 varinfo5_LDFLAGS       = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif

varinfo5so_so_SOURCES   = varinfo5so.c
varinfo5so_so_CFLAGS    = $(AM_CFLAGS) -fpic -O -Wno-shadow
if VGCONF_OS_IS_DARWIN
 varinfo5so_so_LDFLAGS  = -fpic $(AM_FLAG_M3264_PRI) -dynamic \
				-dynamiclib -all_load
else
 varinfo5so_so_LDFLAGS  = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-soname -Wl,varinfo5so.so
endif

# Build shared object for wrap7
wrap7_SOURCES           = wrap7.c
wrap7_DEPENDENCIES      = wrap7so.so
if VGCONF_OS_IS_DARWIN
 wrap7_LDADD            = `pwd`/wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI)
else
 wrap7_LDADD            = wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif

wrap7so_so_SOURCES      = wrap7so.c
wrap7so_so_CFLAGS       = $(AM_CFLAGS) -fpic
if VGCONF_OS_IS_DARWIN
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -dynamic \
				-dynamiclib -all_load
else
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-soname -Wl,wrap7so.so
endif

