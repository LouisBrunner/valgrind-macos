noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	args.stderr.exp args.stdout.exp args.vgtest \
	bitfield1.stderr.exp bitfield1.vgtest \
	bt_everything.stderr.exp \
	bt_everything.stdout.exp bt_everything.vgtest \
	bt_literal.stderr.exp bt_literal.stdout.exp \
	bt_literal.vgtest \
	coolo_sigaction.stderr.exp \
	coolo_sigaction.stdout.exp coolo_sigaction.vgtest \
	coolo_strlen.stderr.exp coolo_strlen.vgtest \
	cpuid.stderr.exp cpuid.stdout.exp cpuid.vgtest \
	dastest.stderr.exp dastest.stdout.exp \
	dastest.vgtest \
	discard.stderr.exp discard.stdout.exp \
	discard.vgtest \
	floored.stderr.exp floored.stdout.exp \
	floored.vgtest \
	fork.stderr.exp fork.stdout.exp fork.vgtest \
	fpu_lazy_eflags.stderr.exp fpu_lazy_eflags.stdout.exp \
	fpu_lazy_eflags.vgtest \
	fucomip.stderr.exp fucomip.vgtest \
	gxx304.stderr.exp gxx304.vgtest \
	munmap_exe.stderr.exp munmap_exe.vgtest \
	pth_blockedsig.stderr.exp \
	pth_blockedsig.stdout.exp pth_blockedsig.vgtest \
	rcl_assert.stderr.exp rcl_assert.vgtest \
	rcrl.stderr.exp rcrl.stdout.exp rcrl.vgtest \
	readline1.stderr.exp readline1.stdout.exp \
	readline1.vgtest \
	resolv.stderr.exp resolv.stdout.exp resolv.vgtest \
	seg_override.stderr.exp \
	seg_override.stdout.exp seg_override.vgtest \
	sha1_test.stderr.exp sha1_test.vgtest \
	shortpush.stderr.exp shortpush.vgtest \
	shorts.stderr.exp shorts.vgtest \
	smc1.stderr.exp smc1.stdout.exp smc1.vgtest

check_PROGRAMS = \
	args bitfield1 bt_everything bt_literal coolo_strlen \
	cpuid dastest discard floored fork fpu_lazy_eflags \
	fucomip munmap_exe rcl_assert \
	rcrl readline1 resolv seg_override sha1_test shortpush shorts smc1 \
	pth_blockedsig \
	coolo_sigaction gxx304

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CXXFLAGS = $(AM_CFLAGS)

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
fpu_lazy_eflags_SOURCES	= fpu_lazy_eflags.c
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

# must be built with these flags -- bug only occurred with them
fpu_lazy_eflags.o: CFLAGS += -O2 -mcpu=pentiumpro -march=pentiumpro

