## Process this file with automake to produce Makefile.in

##---------------------------------------------------------------------------
## These ones all work fine without producing errors in any skin.
##---------------------------------------------------------------------------

noinst_PROGRAMS = \
	bitfield1 bt_everything bt_literal coolo_strlen \
	cpuid dastest floored fork fucomip munmap_exe rcl_assert \
	rcrl readline1 sha1_test shortpush shorts smc1 \
	pth_blockedsig \
	coolo_sigaction gxx304

CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
CXXFLAGS = $(CFLAGS)

# generic C ones
bitfield1_SOURCES 	= bitfield1.c
bt_everything_SOURCES 	= bt_everything.c
bt_literal_SOURCES 	= bt_literal.c
cpuid_SOURCES 		= cpuid_c.c cpuid_s.s
coolo_strlen_SOURCES 	= coolo_strlen.c
dastest_SOURCES 	= dastest_c.c dastest_s.s
fork_SOURCES 		= fork.c
floored_SOURCES 	= floored.c
floored_LDADD 		= -lm
fucomip_SOURCES 	= fucomip.c
munmap_exe_SOURCES 	= munmap_exe.c
rcl_assert_SOURCES 	= rcl_assert.S
rcrl_SOURCES 		= rcrl.c
readline1_SOURCES 	= readline1.c
smc1_SOURCES 		= smc1.c
sha1_test_SOURCES 	= sha1_test.c
shortpush_SOURCES 	= shortpush.c
shorts_SOURCES 		= shorts.c

# pthread C ones
pth_blockedsig_SOURCES	= pth_blockedsig.c
pth_blockedsig_LDADD	= -lpthread

# generic C++ ones
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp


