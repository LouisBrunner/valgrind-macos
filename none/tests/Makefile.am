## Process this file with automake to produce Makefile.in

##---------------------------------------------------------------------------
## These ones all work fine without producing errors in any skin.
##---------------------------------------------------------------------------

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	$(noinst_SCRIPTS) \
	args.stderr.exp args.stderr.exp.hd args.stdout.exp args.vgtest \
	bitfield1.stderr.exp bitfield1.stderr.exp.hd bitfield1.vgtest \
	bt_everything.stderr.exp bt_everything.stderr.exp.hd \
	bt_everything.stdout.exp bt_everything.vgtest \
	bt_literal.stderr.exp bt_literal.stderr.exp.hd bt_literal.stdout.exp \
	bt_literal.vgtest \
	coolo_sigaction.stderr.exp coolo_sigaction.stderr.exp.hd \
	coolo_sigaction.stdout.exp coolo_sigaction.vgtest \
	coolo_strlen.stderr.exp coolo_strlen.stderr.exp.hd coolo_strlen.vgtest \
	cpuid.stderr.exp cpuid.stderr.exp.hd cpuid.stdout.exp cpuid.vgtest \
	dastest.stderr.exp dastest.stderr.exp.hd dastest.stdout.exp \
	dastest.vgtest \
	discard.stderr.exp discard.stderr.exp.hd discard.stdout.exp \
	discard.vgtest \
	floored.stderr.exp floored.stderr.exp.hd floored.stdout.exp \
	floored.vgtest \
	fork.stderr.exp fork.stderr.exp.hd fork.stdout.exp fork.vgtest \
	fucomip.stderr.exp fucomip.stderr.exp.hd fucomip.vgtest \
	gxx304.stderr.exp gxx304.stderr.exp.hd gxx304.vgtest \
	munmap_exe.stderr.exp munmap_exe.stderr.exp.hd munmap_exe.vgtest \
	pth_blockedsig.stderr.exp pth_blockedsig.stderr.exp.hd \
	pth_blockedsig.stdout.exp pth_blockedsig.vgtest \
	rcl_assert.stderr.exp rcl_assert.stderr.exp.hd rcl_assert.vgtest \
	rcrl.stderr.exp rcrl.stderr.exp.hd rcrl.stdout.exp rcrl.vgtest \
	readline1.stderr.exp readline1.stderr.exp.hd readline1.stdout.exp \
	readline1.vgtest \
	resolv.stderr.exp resolv.stderr.exp.hd resolv.stdout.exp resolv.vgtest \
	seg_override.stderr.exp seg_override.stderr.exp.hd \
	seg_override.stdout.exp seg_override.vgtest \
	sha1_test.stderr.exp sha1_test.stderr.exp.hd sha1_test.vgtest \
	shortpush.stderr.exp shortpush.stderr.exp.hd shortpush.vgtest \
	shorts.stderr.exp shorts.stderr.exp.hd shorts.vgtest \
	smc1.stderr.exp smc1.stderr.exp.hd smc1.stdout.exp smc1.vgtest

noinst_PROGRAMS = \
	args bitfield1 bt_everything bt_literal coolo_strlen \
	cpuid dastest discard floored fork fucomip munmap_exe rcl_assert \
	rcrl readline1 resolv seg_override sha1_test shortpush shorts smc1 \
	pth_blockedsig \
	coolo_sigaction gxx304

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CXXFLAGS = $(AM_CFLAGS)
CXXFLAGS    = $(AM_CFLAGS)

# generic C ones
args_SOURCES 		= args.c
bitfield1_SOURCES 	= bitfield1.c
bt_everything_SOURCES 	= bt_everything.c
bt_literal_SOURCES 	= bt_literal.c
cpuid_SOURCES 		= cpuid_c.c cpuid_s.s
coolo_strlen_SOURCES 	= coolo_strlen.c
dastest_SOURCES 	= dastest_c.c dastest_s.s
discard_SOURCES 	= discard.c
fork_SOURCES 		= fork.c
floored_SOURCES 	= floored.c
floored_LDADD 		= -lm
fucomip_SOURCES 	= fucomip.c
munmap_exe_SOURCES 	= munmap_exe.c
rcl_assert_SOURCES 	= rcl_assert.S
rcrl_SOURCES 		= rcrl.c
readline1_SOURCES 	= readline1.c
resolv_SOURCES 		= resolv.c
seg_override_SOURCES 	= seg_override.c
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


