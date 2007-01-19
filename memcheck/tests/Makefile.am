
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .
if VGP_X86_LINUX
SUBDIRS += x86
endif
if VGP_AMD64_LINUX
SUBDIRS += amd64
endif
if VGP_PPC32_LINUX
SUBDIRS += ppc32
endif
if VGP_PPC64_LINUX
SUBDIRS += ppc64
endif

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
	deep_templates.vgtest \
	deep_templates.stdout.exp deep_templates.stderr.exp \
	describe-block.stderr.exp describe-block.vgtest \
	doublefree.stderr.exp doublefree.vgtest \
	erringfds.stderr.exp erringfds.stdout.exp erringfds.vgtest \
	error_counts.stderr.exp error_counts.stdout.exp error_counts.vgtest \
	errs1.stderr.exp errs1.vgtest \
	exitprog.stderr.exp exitprog.vgtest \
	execve.stderr.exp execve.stderr.exp2 execve.vgtest \
	execve2.stderr.exp execve2.stderr.exp2 execve2.vgtest \
	fprw.stderr.exp fprw.vgtest \
	fwrite.stderr.exp fwrite.stderr.exp2 fwrite.vgtest \
	inits.stderr.exp inits.vgtest \
	inline.stderr.exp inline.stdout.exp inline.vgtest \
	leak-0.vgtest leak-0.stderr.exp \
	leak-cycle.vgtest leak-cycle.stderr.exp leak-cycle.stderr.exp64 \
	leak-pool-0.vgtest leak-pool-0.stderr.exp leak-pool-0.stderr.exp64 \
	leak-pool-1.vgtest leak-pool-1.stderr.exp leak-pool-1.stderr.exp64 \
	leak-pool-2.vgtest leak-pool-2.stderr.exp leak-pool-2.stderr.exp64 \
	leak-pool-3.vgtest leak-pool-3.stderr.exp leak-pool-3.stderr.exp64 \
	leak-pool-4.vgtest leak-pool-4.stderr.exp leak-pool-4.stderr.exp64 \
	leak-pool-5.vgtest leak-pool-5.stderr.exp leak-pool-5.stderr.exp64 \
	leak-tree.vgtest leak-tree.stderr.exp \
	leak-tree.stderr.exp2 leak-tree.stderr.exp64 \
	leak-regroot.vgtest leak-regroot.stderr.exp \
	leakotron.vgtest leakotron.stdout.exp leakotron.stderr.exp \
	long_namespace_xml.vgtest long_namespace_xml.stdout.exp \
	long_namespace_xml.stderr.exp \
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
	nanoleak2.stderr.exp nanoleak2.vgtest \
	new_nothrow.stderr.exp new_nothrow.vgtest \
	new_override.stderr.exp new_override.stdout.exp new_override.vgtest \
	null_socket.stderr.exp null_socket.vgtest \
	overlap.stderr.exp overlap.stdout.exp overlap.vgtest \
	oset_test.stderr.exp oset_test.stdout.exp oset_test.vgtest \
	partiallydefinedeq.vgtest partiallydefinedeq.stderr.exp \
	partiallydefinedeq.stderr.exp2 \
	partiallydefinedeq.stdout.exp \
	partial_load_ok.vgtest partial_load_ok.stderr.exp partial_load_ok.stderr.exp64 \
	partial_load_dflt.vgtest partial_load_dflt.stderr.exp partial_load_dflt.stderr.exp64 \
	pdb-realloc.stderr.exp pdb-realloc.vgtest \
	pdb-realloc2.stderr.exp pdb-realloc2.stdout.exp pdb-realloc2.vgtest \
	pipe.stderr.exp pipe.vgtest \
	pointer-trace.vgtest \
	pointer-trace.stderr.exp pointer-trace.stderr.exp64 \
	post-syscall.stderr.exp post-syscall.stdout.exp post-syscall.vgtest \
	realloc1.stderr.exp realloc1.vgtest \
	realloc2.stderr.exp realloc2.vgtest \
	realloc3.stderr.exp realloc3.vgtest \
	sh-mem.stderr.exp sh-mem.vgtest \
	sh-mem-random.stderr.exp sh-mem-random.stdout.exp64 \
	sh-mem-random.stdout.exp sh-mem-random.vgtest \
	sigaltstack.stderr.exp sigaltstack.vgtest \
	sigkill.stderr.exp sigkill.stderr.exp2 sigkill.stderr.exp3 sigkill.vgtest \
	signal2.stderr.exp signal2.stdout.exp signal2.vgtest \
	sigprocmask.stderr.exp sigprocmask.stderr.exp2 sigprocmask.vgtest \
	stack_changes.stderr.exp stack_changes.stdout.exp \
	stack_changes.stdout.exp2 stack_changes.vgtest \
	stack_switch.stderr.exp stack_switch.vgtest \
	strchr.stderr.exp strchr.stderr.exp2 strchr.vgtest \
	str_tester.stderr.exp str_tester.vgtest \
	supp_unknown.stderr.exp supp_unknown.vgtest supp_unknown.supp \
	supp1.stderr.exp supp1.vgtest \
	supp2.stderr.exp supp2.vgtest \
	supp.supp \
	suppfree.stderr.exp suppfree.vgtest \
	toobig-allocs.stderr.exp toobig-allocs.vgtest \
	trivialleak.stderr.exp trivialleak.vgtest \
	metadata.stderr.exp metadata.stdout.exp metadata.vgtest \
	vcpu_bz2.stdout.exp vcpu_bz2.stderr.exp vcpu_bz2.vgtest \
	vcpu_fbench.stdout.exp vcpu_fbench.stderr.exp vcpu_fbench.vgtest \
	vcpu_fnfns.stdout.exp vcpu_fnfns.stderr.exp vcpu_fnfns.vgtest \
	vgtest_ume.stderr.exp vgtest_ume.disabled \
	with-space.stderr.exp with-space.stdout.exp with-space.vgtest \
	wrap1.vgtest wrap1.stdout.exp wrap1.stderr.exp \
	wrap2.vgtest wrap2.stdout.exp wrap2.stderr.exp \
	wrap3.vgtest wrap3.stdout.exp wrap3.stderr.exp \
	wrap4.vgtest wrap4.stdout.exp wrap4.stderr.exp \
	wrap5.vgtest wrap5.stdout.exp wrap5.stderr.exp \
	wrap6.vgtest wrap6.stdout.exp wrap6.stderr.exp \
	wrap7.vgtest wrap7.stdout.exp wrap7.stderr.exp \
	wrap8.vgtest wrap8.stdout.exp wrap8.stderr.exp \
	wrap8.stdout.exp2 wrap8.stderr.exp2 \
	writev.stderr.exp writev.stderr.exp2 writev.stderr.exp3 writev.vgtest \
	xml1.stderr.exp xml1.stderr.exp2 xml1.stderr.exp3 \
	xml1.stderr.exp64 xml1.stderr.exp64_2 xml1.stdout.exp \
	xml1.vgtest \
	zeropage.stderr.exp zeropage.stderr.exp2 zeropage.stdout.exp \
	zeropage.vgtest

# vgtest_ume is not working
check_PROGRAMS = \
	addressable \
	badaddrvalue badfree badjump badjump2 \
	badloop badpoll badrw brk brk2 buflen_check \
	clientperm custom_alloc \
	deep_templates \
	describe-block \
	doublefree error_counts errs1 exitprog execve execve2 erringfds \
	fprw fwrite hello inits inline \
	leak-0 leak-cycle leak-pool leak-tree leak-regroot leakotron \
	long_namespace_xml \
	malloc_usable malloc1 malloc2 malloc3 manuel1 manuel2 manuel3 \
	match-overrun \
	memalign_test memalign2 memcmptest mempool mmaptest \
	nanoleak nanoleak2 new_nothrow \
	null_socket oset_test overlap \
	partiallydefinedeq \
	partial_load pdb-realloc pdb-realloc2 \
	pipe pointer-trace \
	post-syscall \
	realloc1 realloc2 realloc3 \
	sh-mem sh-mem-random \
	sigaltstack signal2 sigprocmask sigkill \
	stack_changes stack_switch strchr str_tester \
	supp_unknown supp1 supp2 suppfree \
	trivialleak \
	mismatches new_override metadata \
	vcpu_bz2 vcpu_fbench vcpu_fnfns \
	xml1 \
	wrap1 wrap2 wrap3 wrap4 wrap5 wrap6 wrap7 wrap7so.so wrap8 \
	writev zeropage


AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include \
		-I$(top_srcdir)/coregrind -I$(top_builddir)/include \
		-I@VEX_DIR@/pub
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS = $(AM_CFLAGS)

# Extra stuff for C tests
memcmptest_CFLAGS	= $(AM_FLAG_M3264_PRI) $(AM_CFLAGS) -fno-builtin-memcmp
oset_test_CFLAGS	= $(AM_FLAG_M3264_PRI) \
				-DVGA_$(VG_ARCH)=1 -DVGO_$(VG_OS)=1 \
				-DVGP_$(VG_ARCH)_$(VG_OS)=1
vcpu_bz2_CFLAGS		= $(AM_FLAG_M3264_PRI) $(AM_CFLAGS) -O2 -g
vcpu_fbench_CFLAGS	= $(AM_FLAG_M3264_PRI) $(AM_CFLAGS) -O2 -g
vcpu_fnfns_CFLAGS	= $(AM_FLAG_M3264_PRI) $(AM_CFLAGS) -O2 -g
vcpu_fnfns_LDADD	= -lm
wrap6_CFLAGS		= $(AM_FLAG_M3264_PRI) $(AM_CFLAGS) -O2 -g

# Don't allow GCC to inline memcpy(), because then we can't intercept it
overlap_CFLAGS		= $(AM_CFLAGS) -fno-builtin-memcpy
stack_switch_LDADD      = -lpthread
str_tester_CFLAGS	= $(AM_CFLAGS) -Wno-shadow
supp_unknown_SOURCES	= badjump.c
supp1_SOURCES		= supp.c
supp2_SOURCES		= supp.c

# C++ tests
mismatches_SOURCES	= mismatches.cpp
new_nothrow_SOURCES 	= new_nothrow.cpp
new_override_SOURCES 	= new_override.cpp
deep_templates_SOURCES	= deep_templates.cpp
long_namespace_xml_SOURCES = long_namespace_xml.cpp
deep_templates_CXXFLAGS	= $(AM_FLAG_M3264_PRI) $(AM_CFLAGS) -O -gstabs

if VGP_PPC32_AIX5
if VGP_PPC64_AIX5
else
 # persuade the AIX linker not to junk apparently unused
 # function descriptors.  Unfortunately -Wl,-G -Wl,-bnogc
 # produces a link error on 64-bit AIX, hence only 32-bit
 # gets these flags.
 wrap1_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
 wrap2_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
 wrap3_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
 wrap4_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
 wrap5_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
 wrap6_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
 wrap8_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
endif
endif

# Build shared object for wrap7
wrap7_SOURCES           = wrap7.c
wrap7_DEPENDENCIES      = wrap7so.so
if VGP_PPC64_AIX5 
 wrap7_LDADD            = `pwd`/wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI)
else
if VGP_PPC32_AIX5
 wrap7_LDADD            = `pwd`/wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
else
 wrap7_LDADD            = wrap7so.so
 wrap7_LDFLAGS          = $(AM_FLAG_M3264_PRI) \
				-Wl,-rpath,$(top_builddir)/memcheck/tests
endif
endif

wrap7so_so_SOURCES      = wrap7so.c
wrap7so_so_LDADD        = 
wrap7so_so_DEPENDENCIES = 
wrap7so_so_CFLAGS       = -fpic $(AM_FLAG_M3264_PRI)
if VGP_PPC64_AIX5
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared
else
if VGP_PPC32_AIX5
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-G -Wl,-bnogc
else
 wrap7so_so_LDFLAGS     = -fpic $(AM_FLAG_M3264_PRI) -shared \
				-Wl,-soname -Wl,wrap7so.so
endif
endif

# Valgrind unit self-tests
#hello_LDFLAGS		= -Wl,-defsym,valt_load_address=0x50000000 \
#			  -Wl,-T,$(top_builddir)/valt_load_address.lds
#hello_DEPENDENCIES 	= $(top_builddir)/valt_load_address.lds

# vgtest_ume is not working
#vgtest_ume_CFLAGS	= -DVGA_$(VG_ARCH) -DVGO_$(VG_OS)
#vgtest_ume_LDADD	= ../../coregrind/m_ume.o

