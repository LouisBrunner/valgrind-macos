noinst_SCRIPTS = filter_stderr filter_pushfpopf filter_tronical

INSN_TESTS=insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpeflags.stderr.exp fpeflags.vgtest \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	pushfpopf.stderr.exp pushfpopf.stdout.exp pushfpopf.vgtest \
	tronical.stderr.exp tronical.vgtest

check_PROGRAMS = \
	fpeflags pushfpopf tronical

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 
AM_CXXFLAGS = $(AM_CFLAGS)

fpeflags_SOURCES	= fpeflags.c
# must be built with these flags -- bug only occurred with them
fpeflags_CFLAGS		= $(AM_CFLAGS) -march=i686
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.s
tronical_SOURCES 	= tronical.S
