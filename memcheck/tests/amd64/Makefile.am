
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

INSN_TESTS = insn_basic insn_mmx insn_sse insn_sse2 insn_fpu

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	bt_everything.stderr.exp bt_everything.stdout.exp \
		bt_everything.vgtest \
	bug132146.vgtest bug132146.stderr.exp bug132146.stdout.exp \
	bug279698.vgtest bug279698.stderr.exp bug279698.stdout.exp \
	fxsave-amd64.vgtest fxsave-amd64.stdout.exp fxsave-amd64.stderr.exp \
	insn-bsfl.vgtest insn-bsfl.stdout.exp insn-bsfl.stderr.exp \
	insn-pcmpistri.vgtest insn-pcmpistri.stdout.exp insn-pcmpistri.stderr.exp \
	insn-pmovmskb.vgtest insn-pmovmskb.stdout.exp insn-pmovmskb.stderr.exp \
		insn-pmovmskb.stderr.exp-clang \
	more_x87_fp.stderr.exp more_x87_fp.stdout.exp more_x87_fp.vgtest \
	pcmpgt.stderr.exp pcmpgt.vgtest \
	sh-mem-vec128-plo-no.vgtest \
		sh-mem-vec128-plo-no.stderr.exp \
		sh-mem-vec128-plo-no.stdout.exp \
	sh-mem-vec128-plo-yes.vgtest \
		sh-mem-vec128-plo-yes.stderr.exp \
		sh-mem-vec128-plo-yes.stdout.exp \
	sh-mem-vec256-plo-no.vgtest \
		sh-mem-vec256-plo-no.stderr.exp \
		sh-mem-vec256-plo-no.stdout.exp \
	sh-mem-vec256-plo-yes.vgtest \
		sh-mem-vec256-plo-yes.stderr.exp \
		sh-mem-vec256-plo-yes.stdout.exp \
	shr_edx.stderr.exp shr_edx.stdout.exp shr_edx.vgtest \
	sse_memory.stderr.exp sse_memory.stdout.exp sse_memory.vgtest \
	xor-undef-amd64.stderr.exp xor-undef-amd64.stdout.exp \
	xor-undef-amd64.stderr.exp-freebsd xor-undef-amd64.vgtest \
	xsave-avx.vgtest xsave-avx.stdout.exp xsave-avx.stderr.exp

check_PROGRAMS = \
	bt_everything \
	bug132146 \
	bug279698 \
	fxsave-amd64 \
	insn-bsfl \
	insn-pmovmskb \
	pcmpgt \
	sh-mem-vec128 \
	sse_memory \
	xor-undef-amd64

if BUILD_AVX_TESTS
 check_PROGRAMS += sh-mem-vec256 xsave-avx
endif
if HAVE_ASM_CONSTRAINT_P
 check_PROGRAMS += insn-pcmpistri
endif
# clang 3.5.0 barfs about -mfancy-math-387
if !COMPILER_IS_CLANG
 check_PROGRAMS += \
 	more_x87_fp
endif

# initially this was a "not clang" test
# but the problem is macOS, not clang
if !VGCONF_OS_IS_DARWIN
 check_PROGRAMS += \
	shr_edx
endif

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

fxsave_amd64_CFLAGS	= $(AM_CFLAGS) @FLAG_NO_PIE@
insn_pcmpistri_CFLAGS	= $(AM_CFLAGS)
more_x87_fp_CFLAGS	= $(AM_CFLAGS) -O -ffast-math -mfpmath=387 \
				-mfancy-math-387
more_x87_fp_LDADD	= -lm
pcmpgt_SOURCES = pcmpgt.cpp
shr_edx_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@ @FLAG_W_NO_UNINITIALIZED@
