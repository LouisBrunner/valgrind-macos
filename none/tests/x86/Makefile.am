noinst_SCRIPTS = filter_cpuid filter_stderr gen_insn_test.pl

CLEANFILES = $(addsuffix .c,$(INSN_TESTS))
INSN_TESTS=insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	badseg.stderr.exp badseg.stdout.exp badseg.vgtest \
	bt_everything.stderr.exp bt_everything.stdout.exp bt_everything.vgtest \
	bt_literal.stderr.exp bt_literal.stdout.exp bt_literal.vgtest \
	cpuid.stderr.exp cpuid.stdout.exp cpuid.vgtest \
	fpu_lazy_eflags.stderr.exp fpu_lazy_eflags.stdout.exp \
	fpu_lazy_eflags.vgtest \
	getseg.stdout.exp getseg.stderr.exp getseg.vgtest \
	int.stderr.exp int.stdout.exp int.vgtest \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	pushpopseg.stderr.exp pushpopseg.stdout.exp pushpopseg.vgtest \
	seg_override.stderr.exp seg_override.stdout.exp seg_override.vgtest \
	sigcontext.stdout.exp sigcontext.stderr.exp sigcontext.vgtest

check_PROGRAMS = \
	badseg bt_everything bt_literal cpuid fpu_lazy_eflags \
	getseg $(INSN_TESTS) \
	int pushpopseg seg_override sigcontext

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
badseg_SOURCES		= badseg.c
bt_everything_SOURCES 	= bt_everything.c
bt_literal_SOURCES 	= bt_literal.c
cpuid_SOURCES 		= cpuid_c.c cpuid_s.s
fpu_lazy_eflags_SOURCES	= fpu_lazy_eflags.c
getseg_SOURCES 		= getseg.c
insn_basic_SOURCES	= insn_basic.def
insn_basic_LDADD	= -lm
insn_fpu_SOURCES	= insn_fpu.def
insn_fpu_LDADD		= -lm
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
int_SOURCES		= int.c
pushpopseg_SOURCES	= pushpopseg.c
seg_override_SOURCES 	= seg_override.c
sigcontext_SOURCES	= sigcontext.c

.def.c: $(srcdir)/gen_insn_test.pl
	$(PERL) $(srcdir)/gen_insn_test.pl < $< > $@
