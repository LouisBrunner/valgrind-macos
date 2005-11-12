noinst_SCRIPTS = 

noinst_HEADERS = 

INSN_TESTS = insn_basic insn_mmx insn_sse insn_sse2 insn_fpu

EXTRA_DIST = $(noinst_SCRIPTS) \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS))

check_PROGRAMS = 

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)
