noinst_SCRIPTS = filter_cpuid  filter_none_discards filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	args.stderr.exp args.stdout.exp args.vgtest \
	bitfield1.stderr.exp bitfield1.vgtest \
	bt_everything.stderr.exp \
	bt_everything.stdout.exp bt_everything.vgtest \
	bt_literal.stderr.exp bt_literal.stdout.exp \
	bt_literal.vgtest \
	closeall.stderr.exp closeall.vgtest \
	coolo_sigaction.stderr.exp \
	coolo_sigaction.stdout.exp coolo_sigaction.vgtest \
	coolo_strlen.stderr.exp coolo_strlen.vgtest \
	cpuid.stderr.exp cpuid.stdout.exp cpuid.vgtest \
	dastest.stderr.exp dastest.stdout.exp \
	dastest.vgtest \
	discard.stderr.exp discard.stdout.exp \
	discard.vgtest \
	exec-sigmask.vgtest 
	exec-sigmask.stdout.exp exec-sigmask.stderr.exp \
	floored.stderr.exp floored.stdout.exp \
	floored.vgtest \
	fork.stderr.exp fork.stdout.exp fork.vgtest \
	fpu_lazy_eflags.stderr.exp fpu_lazy_eflags.stdout.exp \
	fpu_lazy_eflags.vgtest \
	fucomip.stderr.exp fucomip.vgtest \
	gxx304.stderr.exp gxx304.vgtest \
	insn_basic.stderr.exp insn_basic.stdout.exp insn_basic.vgtest \
	insn_cmov.stderr.exp insn_cmov.stdout.exp insn_cmov.vgtest \
	insn_mmx.stderr.exp insn_mmx.stdout.exp insn_mmx.vgtest \
	insn_mmxext.stderr.exp insn_mmxext.stdout.exp insn_mmxext.vgtest \
	insn_sse.stderr.exp insn_sse.stdout.exp insn_sse.vgtest \
	insn_sse2.stderr.exp insn_sse2.stdout.exp insn_sse2.vgtest \
	map_unmap.stdout.exp map_unmap.vgtest \
	mremap.stdout.exp mremap.vgtest \
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
	tls.stderr.exp tls.stdout.exp tls.vgtest \
	smc1.stderr.exp smc1.stdout.exp smc1.vgtest \
	syscall-restart1.vgtest syscall-restart1.stdout.exp syscall-restart1.stderr.exp \
	syscall-restart2.vgtest syscall-restart2.stdout.exp syscall-restart2.stderr.exp \
	system.stdout.exp system.vgtest
	yield.stdout.exp yield.vgtest

check_PROGRAMS = \
	args bitfield1 bt_everything bt_literal closeall coolo_strlen \
	cpuid dastest discard exec-sigmask floored fork fpu_lazy_eflags \
	fucomip insn_basic insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2 \
	munmap_exe map_unmap mremap rcl_assert \
	rcrl readline1 resolv seg_override sha1_test shortpush shorts smc1 \
	pth_blockedsig \
	syscall-restart1 syscall-restart2 system \
	coolo_sigaction gxx304 yield

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
args_SOURCES 		= args.c
bitfield1_SOURCES 	= bitfield1.c
bt_everything_SOURCES 	= bt_everything.c
bt_literal_SOURCES 	= bt_literal.c
closeall_SOURCES	= closeall.c
cpuid_SOURCES 		= cpuid_c.c cpuid_s.s
coolo_strlen_SOURCES 	= coolo_strlen.c
dastest_SOURCES 	= dastest_c.c dastest_s.s
discard_SOURCES 	= discard.c
exec_sigmask_SOURCES	= exec-sigmask.c
fork_SOURCES 		= fork.c
floored_SOURCES 	= floored.c
floored_LDADD 		= -lm
fpu_lazy_eflags_SOURCES	= fpu_lazy_eflags.c
fucomip_SOURCES 	= fucomip.c
insn_basic_SOURCES	= insn_basic.def
insn_basic_LDADD	= -lm
insn_cmov_SOURCES	= insn_cmov.def
insn_cmov_LDADD		= -lm
insn_mmx_SOURCES	= insn_mmx.def
insn_mmx_LDADD		= -lm
insn_mmxext_SOURCES	= insn_mmxext.def
insn_mmxext_LDADD	= -lm
insn_sse_SOURCES	= insn_sse.def
insn_sse_LDADD		= -lm
insn_sse2_SOURCES	= insn_sse2.def
insn_sse2_LDADD		= -lm
map_unmap_SOURCES	= map_unmap.c
mremap_SOURCES		= mremap.c
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
syscall_restart1_SOURCES = syscall-restart1.c
syscall_restart2_SOURCES = syscall-restart2.c
system_SOURCES 		= system.c
#tls_SOURCES		= tls.c tls2.c
#tls_DEPENDENCIES	= tls.so
#tls_LDFLAGS		= -Wl,-rpath,$(srcdir)
#tls_LDADD		= tls.so -lpthread
#tls_so_SOURCES		= tls_so.c
#tls_so_LDADD		= tls2.so
#tls_so_DEPENDENCIES	= tls2.so
#tls_so_LDFLAGS		= -Wl,-rpath,$(srcdir) -shared
#tls2_so_SOURCES		= tls2_so.c
#tls2_so_LDFLAGS		= -shared
yield_SOURCES		= yield.c
yield_LDADD		= -lpthread

# pthread C ones
pth_blockedsig_SOURCES	= pth_blockedsig.c
pth_blockedsig_LDADD	= -lpthread

# generic C++ ones
coolo_sigaction_SOURCES	= coolo_sigaction.cpp
gxx304_SOURCES		= gxx304.cpp

# must be built with these flags -- bug only occurred with them
fpu_lazy_eflags.o: CFLAGS += -O2 -mcpu=pentiumpro -march=pentiumpro

# rebuild instruction tests if test generator changes
insn_basic.c: gen_insn_test.pl
insn_cmov.c: gen_insn_test.pl
insn_mmx.c: gen_insn_test.pl
insn_mmxext.c: gen_insn_test.pl
insn_sse.c: gen_insn_test.pl
insn_sse2.c: gen_insn_test.pl

.def.c:
	$(PERL) gen_insn_test.pl < $< > $@
