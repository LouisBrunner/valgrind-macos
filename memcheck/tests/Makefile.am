## Process this file with automake to produce Makefile.in

##---------------------------------------------------------------------------
## Need more tests:
## - lots more mmap/munmap/mremap/mprotect ones

## Notes:
##   - HEAD and ERASER stderr.exp are different for signal2 due to lazy vs.
##     strict EIP updating
##---------------------------------------------------------------------------

noinst_PROGRAMS = \
	badaddrvalue badfree badjump badloop buflen_check \
	doublefree errs1 exitprog fprw fwrite inits inline \
	malloc1 malloc2 manuel1 manuel2 manuel3 \
	memalign_test memcmptest mmaptest nanoleak pushfpopf \
	realloc1 realloc2 sigaltstack signal2 supp1 supp2 suppfree \
	trivialleak tronical weirdioctl	\
	mismatches new_override

CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
CXXFLAGS = $(CFLAGS)

# C ones
badaddrvalue_SOURCES 	= badaddrvalue.c
badfree_SOURCES 	= badfree.c
badjump_SOURCES 	= badjump.c
badloop_SOURCES 	= badloop.c
buflen_check_SOURCES	= buflen_check.c
doublefree_SOURCES 	= doublefree.c
errs1_SOURCES 		= errs1.c
exitprog_SOURCES 	= exitprog.c
fprw_SOURCES 		= fprw.c
fwrite_SOURCES 		= fwrite.c
inits_SOURCES		= inits.c
inline_SOURCES 	        = inline.c
malloc1_SOURCES 	= malloc1.c
malloc2_SOURCES 	= malloc2.c
manuel1_SOURCES 	= manuel1.c
manuel2_SOURCES 	= manuel2.c
manuel3_SOURCES 	= manuel3.c
mmaptest_SOURCES 	= mmaptest.c
memalign_test_SOURCES 	= memalign_test.c
memcmptest_SOURCES 	= memcmptest.c
nanoleak_SOURCES 	= nanoleak.c
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.s
realloc1_SOURCES 	= realloc1.c
realloc2_SOURCES 	= realloc2.c
signal2_SOURCES 	= signal2.c
supp1_SOURCES 		= supp.c
supp2_SOURCES 		= supp.c
suppfree_SOURCES 	= suppfree.c
sigaltstack_SOURCES 	= sigaltstack.c
trivialleak_SOURCES 	= trivialleak.c
tronical_SOURCES 	= tronical.S
weirdioctl_SOURCES 	= weirdioctl.c

# C++ ones
mismatches_SOURCES	= mismatches.cpp
new_override_SOURCES 	= new_override.cpp

