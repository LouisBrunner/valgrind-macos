noinst_SCRIPTS = filter_stderr

INSN_TESTS=insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpu-28-108.vgtest fpu-28-108.stderr.exp \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS))

check_PROGRAMS = \
	fpu-28-108

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g

fpu_28_108_SOURCES	= fpu-28-108.S
