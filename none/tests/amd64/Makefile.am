
noinst_SCRIPTS = filter_cpuid filter_stderr filter_int gen_insn_test.pl

CLEANFILES = $(addsuffix .c,$(INSN_TESTS))
INSN_TESTS = insn_basic insn_mmx insn_sse insn_sse2 insn_fpu

EXTRA_DIST = $(noinst_SCRIPTS) \
	faultstatus.vgtest faultstatus.stderr.exp \
	fcmovnu.vgtest fcmovnu.stderr.exp fcmovnu.stdout.exp \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	jrcxz.stderr.exp jrcxz.stdout.exp jrcxz.vgtest \
	looper.stderr.exp looper.stdout.exp looper.vgtest \
        smc1.stderr.exp smc1.stdout.exp smc1.vgtest \
        shrld.stderr.exp shrld.stdout.exp shrld.vgtest


check_PROGRAMS = \
	faultstatus fcmovnu $(INSN_TESTS) looper jrcxz smc1 shrld

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
		@FLAG_M64@
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
insn_basic_SOURCES	= insn_basic.def
insn_basic_LDADD	= -lm
insn_mmx_SOURCES	= insn_mmx.def
insn_mmx_LDADD		= -lm
insn_sse_SOURCES	= insn_sse.def
insn_sse_LDADD		= -lm
insn_sse2_SOURCES	= insn_sse2.def
insn_sse2_LDADD		= -lm
insn_fpu_SOURCES	= insn_fpu.def
insn_fpu_LDADD		= -lm

.def.c: $(srcdir)/gen_insn_test.pl
	$(PERL) $(srcdir)/gen_insn_test.pl < $< > $@
