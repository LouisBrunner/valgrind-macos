
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_pushfpopf filter_tronical

INSN_TESTS = insn_basic insn_fpu insn_cmov insn_mmx insn_mmxext insn_sse insn_sse2

EXTRA_DIST = \
	bug152022.vgtest bug152022.stderr.exp bug152022.stdout.exp \
	espindola2.vgtest espindola2.stderr.exp \
	fpeflags.stderr.exp fpeflags.vgtest \
	fxsave.vgtest fxsave.stdout.exp fxsave.stderr.exp \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	pushfpopf.stderr.exp pushfpopf.stdout.exp pushfpopf.vgtest \
	pushfw_x86.vgtest pushfw_x86.stdout.exp pushfw_x86.stderr.exp \
	pushpopmem.stderr.exp pushpopmem.stdout.exp pushpopmem.vgtest \
	sh-mem-vec128-plo-no.vgtest \
		sh-mem-vec128-plo-no.stderr.exp \
		sh-mem-vec128-plo-no.stdout.exp \
	sh-mem-vec128-plo-yes.vgtest \
		sh-mem-vec128-plo-yes.stderr.exp \
		sh-mem-vec128-plo-yes.stdout.exp \
	sse1_memory.stderr.exp sse1_memory.stdout.exp sse1_memory.vgtest \
	sse2_memory.stderr.exp sse2_memory.stdout.exp sse2_memory.vgtest \
	tronical.stderr.exp tronical.vgtest \
	more_x86_fp.stderr.exp more_x86_fp.stdout.exp more_x86_fp.vgtest \
	fprem.stderr.exp fprem.stdout.exp fprem.vgtest \
	xor-undef-x86.stderr.exp xor-undef-x86.stdout.exp \
	xor-undef-x86.vgtest

check_PROGRAMS = \
	bug152022 \
	espindola2 \
	fpeflags \
	fprem \
	fxsave \
	more_x86_fp \
	pushfpopf \
	pushfw_x86 \
	pushpopmem \
	sh-mem-vec128 \
	sse_memory \
	tronical \
	xor-undef-x86

AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@


# fpeflags must use these flags -- bug only occurred with them.
fpeflags_CFLAGS		= $(AM_CFLAGS) -march=i686
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.S
if VGCONF_OS_IS_DARWIN
pushpopmem_CFLAGS 	= $(AM_CFLAGS) -mdynamic-no-pic
else
pushpopmem_CFLAGS 	= $(AM_CFLAGS)
endif
tronical_SOURCES 	= tronical.S

more_x86_fp_LDADD	= -lm
