noinst_SCRIPTS = filter_stderr filter_pushfpopf filter_tronical

noinst_HEADERS = scalar.h

INSN_TESTS = insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = $(noinst_SCRIPTS) \
	bug133694.vgtest bug133694.stderr.exp bug133694.stdout.exp \
	espindola2.vgtest espindola2.stderr.exp \
	fpeflags.stderr.exp fpeflags.vgtest \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	int3-x86.vgtest int3-x86.stderr.exp int3-x86.stdout.exp \
	pushfpopf.stderr.exp pushfpopf.stdout.exp pushfpopf.vgtest \
	pushpopmem.stderr.exp pushpopmem.stdout.exp pushpopmem.vgtest \
	scalar.stderr.exp scalar.stderr.exp2 scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_exit_group.stderr.exp scalar_exit_group.stderr.exp2 \
	scalar_exit_group.vgtest \
	scalar_supp.stderr.exp scalar_supp.stderr.exp2 \
	scalar_supp.vgtest scalar_supp.supp \
	scalar_vfork.stderr.exp scalar_vfork.vgtest \
	sse1_memory.stderr.exp sse1_memory.stdout.exp sse1_memory.vgtest \
	sse2_memory.stderr.exp sse2_memory.stdout.exp sse2_memory.vgtest \
	tronical.stderr.exp tronical.vgtest \
	more_x86_fp.stderr.exp more_x86_fp.stdout.exp more_x86_fp.vgtest \
	fprem.stderr.exp fprem.stdout.exp fprem.vgtest \
	xor-undef-x86.stderr.exp xor-undef-x86.stdout.exp \
	xor-undef-x86.vgtest

check_PROGRAMS = \
	bug133694 \
	espindola2 \
	int3-x86 \
	scalar_exit_group scalar_fork scalar_supp scalar_vfork \
	fpeflags pushfpopf pushpopmem scalar sse_memory tronical \
	more_x86_fp fprem xor-undef-x86

AM_CPPFLAGS  = -I$(top_srcdir)/include
AM_CFLAGS    = $(WERROR) @FLAG_M32@ -Winline -Wall -Wshadow -g \
		$(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  = $(AM_CFLAGS)
AM_CCASFLAGS = @FLAG_M32@

# must be built with these flags -- bug only occurred with them
fpeflags_CFLAGS		= $(AM_CFLAGS) -march=i686
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.s
tronical_SOURCES 	= tronical.S

more_x86_fp_LDADD	= -lm
