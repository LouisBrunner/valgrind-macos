##---------------------------------------------------------------------------
## Need more tests:
## - lots more mmap/munmap/mremap/mprotect ones
##---------------------------------------------------------------------------

noinst_SCRIPTS = filter_allocs filter_leak_check_size \
		 filter_stderr filter_stderr_backtrace filter_pushfpopf \
		 filter_tronical

INSN_TESTS=insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	badaddrvalue.stderr.exp \
	badaddrvalue.stdout.exp badaddrvalue.vgtest \
	badfree-2trace.stderr.exp badfree-2trace.vgtest \
	badfree.stderr.exp badfree.vgtest \
	badjump.stderr.exp badjump.vgtest \
	badloop.stderr.exp badloop.vgtest \
	badrw.stderr.exp badrw.vgtest \
	brk.stderr.exp brk.vgtest \
	buflen_check.stderr.exp buflen_check.vgtest \
	clientperm.stderr.exp \
	clientperm.stdout.exp clientperm.vgtest \
	custom_alloc.stderr.exp custom_alloc.vgtest \
	doublefree.stderr.exp doublefree.vgtest \
	error_counts.stderr.exp error_counts.stdout.exp error_counts.vgtest \
	errs1.stderr.exp errs1.vgtest \
	exitprog.stderr.exp exitprog.vgtest \
	execve.stderr.exp execve.vgtest \
	execve2.stderr.exp execve2.vgtest \
	fpeflags.stderr.exp fpeflags.vgtest \
	fprw.stderr.exp fprw.vgtest \
	fwrite.stderr.exp fwrite.stdout.exp fwrite.vgtest \
	inits.stderr.exp inits.vgtest \
	inline.stderr.exp inline.stdout.exp inline.vgtest \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	malloc1.stderr.exp malloc1.vgtest \
	malloc2.stderr.exp malloc2.vgtest \
	malloc3.stderr.exp malloc3.stdout.exp malloc3.vgtest \
	manuel1.stderr.exp manuel1.stdout.exp manuel1.vgtest \
	manuel2.stderr.exp manuel2.stdout.exp manuel2.vgtest \
	manuel3.stderr.exp manuel3.vgtest \
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
	pth_once.stderr.exp pth_once.stdout.exp pth_once.vgtest \
	pushfpopf.stderr.exp pushfpopf.stdout.exp pushfpopf.vgtest \
	realloc1.stderr.exp realloc1.vgtest \
	realloc2.stderr.exp realloc2.vgtest \
	realloc3.stderr.exp realloc3.vgtest \
	sigaltstack.stderr.exp sigaltstack.vgtest \
	signal2.stderr.exp \
	signal2.stdout.exp signal2.vgtest \
	supp1.stderr.exp supp1.vgtest \
	supp2.stderr.exp supp2.vgtest \
	supp.supp \
	suppfree.stderr.exp suppfree.vgtest \
	toobig-allocs.stderr.exp toobig-allocs.vgtest \
	trivialleak.stderr.exp trivialleak.vgtest \
	tronical.stderr.exp tronical.vgtest \
	weirdioctl.stderr.exp weirdioctl.stdout.exp weirdioctl.vgtest \
	metadata.stderr.exp metadata.stdout.exp metadata.vgtest \
	threadederrno.stderr.exp threadederrno.stdout.exp \
	threadederrno.vgtest \
	writev.stderr.exp writev.vgtest \
	zeropage.stderr.exp zeropage.vgtest

check_PROGRAMS = \
	badaddrvalue badfree badjump badloop badrw brk buflen_check \
	clientperm custom_alloc \
	doublefree error_counts errs1 exitprog execve execve2 \
	fpeflags fprw fwrite inits inline \
	malloc1 malloc2 malloc3 manuel1 manuel2 manuel3 \
	memalign_test memalign2 memcmptest mempool mmaptest \
	nanoleak new_nothrow \
	null_socket overlap pushfpopf \
	realloc1 realloc2 realloc3 sigaltstack signal2 supp1 supp2 suppfree \
	trivialleak tronical weirdioctl	\
	mismatches new_override metadata threadederrno writev zeropage

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 
AM_CXXFLAGS = $(AM_CFLAGS)

# C ones
badaddrvalue_SOURCES 	= badaddrvalue.c
badfree_SOURCES 	= badfree.c
badjump_SOURCES 	= badjump.c
badloop_SOURCES 	= badloop.c
badrw_SOURCES		= badrw.c
brk_SOURCES 		= brk.c
buflen_check_SOURCES	= buflen_check.c
clientperm_SOURCES 	= clientperm.c
custom_alloc_SOURCES 	= custom_alloc.c
doublefree_SOURCES 	= doublefree.c
error_counts_SOURCES 	= error_counts.c
errs1_SOURCES 		= errs1.c
execve_SOURCES 		= execve.c
execve2_SOURCES 	= execve2.c
exitprog_SOURCES 	= exitprog.c
fpeflags_SOURCES	= fpeflags.c
fprw_SOURCES 		= fprw.c
fwrite_SOURCES 		= fwrite.c
inits_SOURCES		= inits.c
inline_SOURCES 	        = inline.c
malloc1_SOURCES 	= malloc1.c
malloc2_SOURCES 	= malloc2.c
malloc3_SOURCES 	= malloc3.c
manuel1_SOURCES 	= manuel1.c
manuel2_SOURCES 	= manuel2.c
manuel3_SOURCES 	= manuel3.c
mmaptest_SOURCES 	= mmaptest.c
memalign_test_SOURCES 	= memalign_test.c
memalign2_SOURCES 	= memalign2.c
memcmptest_SOURCES 	= memcmptest.c
mempool_SOURCES 	= mempool.c
nanoleak_SOURCES 	= nanoleak.c
null_socket_SOURCES 	= null_socket.c
overlap_SOURCES 	= overlap.c
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.s
realloc1_SOURCES 	= realloc1.c
realloc2_SOURCES 	= realloc2.c
realloc3_SOURCES 	= realloc3.c
signal2_SOURCES 	= signal2.c
supp1_SOURCES 		= supp.c
supp2_SOURCES 		= supp.c
suppfree_SOURCES 	= suppfree.c
sigaltstack_SOURCES 	= sigaltstack.c
trivialleak_SOURCES 	= trivialleak.c
tronical_SOURCES 	= tronical.S
weirdioctl_SOURCES 	= weirdioctl.c
metadata_SOURCES	= metadata.c
threadederrno_SOURCES	= threadederrno.c
threadederrno_LDADD	= -lpthread
writev_SOURCES		= writev.c
zeropage_SOURCES	= zeropage.c

# C++ ones
mismatches_SOURCES	= mismatches.cpp
new_nothrow_SOURCES 	= new_nothrow.cpp
new_override_SOURCES 	= new_override.cpp

# must be built with these flags -- bug only occurred with them
fpeflags.o: CFLAGS += -march=i686
