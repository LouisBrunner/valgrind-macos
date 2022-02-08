
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_cpuid filter_inf_nan filter_stderr gen_insn_test.pl

# Used by avx-1.c and avx_estimate_insn.c
noinst_HEADERS = avx_tests.h

CLEANFILES = $(addsuffix .c,$(INSN_TESTS))

INSN_TESTS = insn_basic insn_mmx insn_sse insn_sse2 insn_fpu
if BUILD_SSE3_TESTS
 INSN_TESTS += insn_sse3
endif
if BUILD_SSSE3_TESTS
 INSN_TESTS += insn_ssse3
endif
if BUILD_PCLMULQDQ_TESTS
 INSN_TESTS += insn_pclmulqdq
endif

# Explicitly include insn_sse3 even if ! BUILD_SSE3_TESTS, 
# to avoid packaging screwups if 'make dist' is run on a machine
# which failed the BUILD_SSE3_TESTS test in configure.in.

EXTRA_DIST = \
	aes.vgtest aes.stdout.exp aes.stderr.exp \
	amd64locked.vgtest amd64locked.stdout.exp amd64locked.stderr.exp \
	avx-1.vgtest avx-1.stdout.exp avx-1.stderr.exp \
	avx_estimate_insn.vgtest avx_estimate_insn.stderr.exp \
	avx_estimate_insn.stdout.exp avx_estimate_insn.stdout.exp-amd \
	avx2-1.vgtest avx2-1.stdout.exp avx2-1.stderr.exp \
	asorep.stderr.exp asorep.stdout.exp asorep.vgtest \
	bmi.stderr.exp bmi.stdout.exp bmi.vgtest \
	bt_flags.stderr.exp bt_flags.stdout.exp bt_flags.vgtest \
	fma.stderr.exp fma.stdout.exp fma.vgtest \
	bug127521-64.vgtest bug127521-64.stdout.exp bug127521-64.stderr.exp \
	bug132813-amd64.vgtest bug132813-amd64.stdout.exp \
	bug132813-amd64.stderr.exp \
	bug137714-amd64.vgtest bug137714-amd64.stdout.exp \
	bug137714-amd64.stderr.exp \
	bug132918.vgtest bug132918.stderr.exp bug132918.stdout.exp \
	bug132918.stdout.exp-older-glibc \
	bug156404-amd64.vgtest bug156404-amd64.stdout.exp \
	bug156404-amd64.stderr.exp \
	cet_nops.vgtest cet_nops.stdout.exp cet_nops.stderr.exp \
	clc.vgtest clc.stdout.exp clc.stderr.exp \
	crc32.vgtest crc32.stdout.exp crc32.stderr.exp \
	cmpxchg.vgtest cmpxchg.stdout.exp cmpxchg.stderr.exp \
	f16c.vgtest f16c.stderr.exp f16c.stdout.exp \
	faultstatus.disabled faultstatus.stderr.exp \
	fb_test_amd64.vgtest \
	fb_test_amd64.stderr.exp fb_test_amd64.stdout.exp \
	fb_test_amd64.h fb_test_amd64_muldiv.h fb_test_amd64_shift.h \
	fcmovnu.vgtest fcmovnu.stderr.exp fcmovnu.stdout.exp \
	fma4.vgtest fma4.stdout.exp fma4.stderr.exp \
	fxtract.vgtest fxtract.stderr.exp fxtract.stdout.exp \
	fxtract.stdout.exp-older-glibc \
	getseg.stdout.exp getseg.stderr.exp getseg.vgtest \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	insn_pclmulqdq.vgtest insn_pclmulqdq.stdout.exp \
	insn_pclmulqdq.stderr.exp \
	insn_sse3.stdout.exp insn_sse3.stderr.exp insn_sse3.vgtest \
	insn_ssse3.stdout.exp insn_ssse3.stderr.exp insn_ssse3.vgtest \
	jrcxz.stderr.exp jrcxz.stdout.exp jrcxz.vgtest \
	looper.stderr.exp looper.stdout.exp looper.vgtest \
	loopnel.stderr.exp loopnel.stdout.exp loopnel.vgtest \
	lzcnt64.stderr.exp lzcnt64.stdout.exp lzcnt64.vgtest \
	movbe.stderr.exp movbe.stdout.exp movbe.vgtest \
	mpx.stderr.exp mpx.stdout.exp mpx.vgtest \
	nan80and64.stderr.exp nan80and64.stdout.exp nan80and64.vgtest \
	nibz_bennee_mmap.stderr.exp nibz_bennee_mmap.stdout.exp \
	nibz_bennee_mmap.vgtest \
	pcmpstr64.stderr.exp pcmpstr64.stdout.exp \
	pcmpstr64.vgtest \
	pcmpstr64w.stderr.exp pcmpstr64w.stdout.exp \
	pcmpstr64w.vgtest \
	pcmpxstrx64.stderr.exp pcmpxstrx64.stdout.exp \
	pcmpxstrx64.vgtest \
	pcmpxstrx64w.stderr.exp pcmpxstrx64w.stdout.exp \
	pcmpxstrx64w.vgtest \
	rcl-amd64.vgtest rcl-amd64.stdout.exp rcl-amd64.stderr.exp \
	rdrand.vgtest rdrand.stdout.exp rdrand.stderr.exp \
	rdseed.vgtest rdseed.stdout.exp rdseed.stderr.exp \
	redundantRexW.vgtest redundantRexW.stdout.exp \
	redundantRexW.stderr.exp \
	smc1.stderr.exp smc1.stdout.exp smc1.vgtest \
	sbbmisc.stderr.exp sbbmisc.stdout.exp sbbmisc.vgtest \
	shrld.stderr.exp shrld.stdout.exp shrld.vgtest \
	ssse3_misaligned.stderr.exp ssse3_misaligned.stdout.exp \
	ssse3_misaligned.vgtest \
	sse4-64.stderr.exp sse4-64.stdout.exp sse4-64.vgtest \
	sse4-64.stdout.exp-older-glibc \
	slahf-amd64.stderr.exp slahf-amd64.stdout.exp \
	slahf-amd64.vgtest \
	tm1.vgtest tm1.stderr.exp tm1.stdout.exp \
	x87trigOOR.vgtest x87trigOOR.stderr.exp x87trigOOR.stdout.exp \
	xacq_xrel.stderr.exp xacq_xrel.stdout.exp xacq_xrel.vgtest \
	xadd.stderr.exp xadd.stdout.exp xadd.vgtest \
	sse4-64.stdout.exp.freebsd sse4-64.stdout.exp-freebsd-clang

check_PROGRAMS = \
	allexec \
	amd64locked \
	bt_flags \
	bug127521-64 bug132813-amd64 bug132918 bug137714-amd64 \
	cet_nops \
	clc \
	cmpxchg \
	getseg \
	$(INSN_TESTS) \
	nan80and64 \
	rcl-amd64 \
	redundantRexW \
	smc1 \
	sbbmisc \
	nibz_bennee_mmap \
	x87trigOOR \
	xadd
if BUILD_ADDR32_TESTS
 check_PROGRAMS += asorep
endif
if BUILD_ADX_TESTS
  check_PROGRAMS += fb_test_amd64
endif
if BUILD_AVX_TESTS
if BUILD_VPCLMULQDQ_TESTS
  check_PROGRAMS += avx-1 \
		    avx_estimate_insn
endif
if BUILD_FMA4_TESTS
  check_PROGRAMS += fma4
endif
endif
if BUILD_AVX2_TESTS
if !COMPILER_IS_ICC
  check_PROGRAMS += avx2-1
endif
endif
if BUILD_SSSE3_TESTS
 check_PROGRAMS += ssse3_misaligned
endif
if BUILD_LZCNT_TESTS
 check_PROGRAMS += lzcnt64
endif
if BUILD_MOVBE_TESTS
 check_PROGRAMS += movbe
endif
if BUILD_SSE42_TESTS
 check_PROGRAMS += \
	pcmpstr64 pcmpxstrx64 sse4-64 crc32 aes \
	pcmpstr64w pcmpxstrx64w
endif
if BUILD_TSX_TESTS
  check_PROGRAMS += tm1 xacq_xrel
endif
if BUILD_BMI_TESTS
 check_PROGRAMS += bmi
endif
if BUILD_FMA_TESTS
 check_PROGRAMS += fma
endif
if BUILD_MPX_TESTS
 check_PROGRAMS += mpx
endif
if BUILD_F16C_TESTS
 check_PROGRAMS += f16c
endif
if BUILD_RDRAND_TESTS
 check_PROGRAMS += rdrand
endif
if BUILD_RDSEED_TESTS
 check_PROGRAMS += rdseed
endif


# DDD: these need to be made to work on Darwin like the x86/ ones were.
if ! VGCONF_OS_IS_DARWIN
   check_PROGRAMS += \
	bug156404-amd64 \
	faultstatus \
	fcmovnu \
	fxtract \
	looper \
	jrcxz \
	shrld \
	slahf-amd64
if BUILD_LOOPNEL_TESTS
   check_PROGRAMS += loopnel
endif
endif

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

# generic C ones
amd64locked_CFLAGS	= $(AM_CFLAGS) -O
bug132918_LDADD		= -lm
cmpxchg_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
fb_test_amd64_CFLAGS	= $(AM_CFLAGS) -O -fno-strict-aliasing
fb_test_amd64_LDADD	= -lm
fcmovnu_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
fxtract_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_OVERFLOW@ @FLAG_NO_PIE@
insn_basic_SOURCES	= insn_basic.def
insn_basic_LDADD	= -lm
insn_mmx_SOURCES	= insn_mmx.def
insn_mmx_LDADD		= -lm
insn_sse_SOURCES	= insn_sse.def
insn_sse_LDADD		= -lm
insn_sse2_SOURCES	= insn_sse2.def
insn_sse2_LDADD		= -lm
insn_sse3_SOURCES	= insn_sse3.def
insn_sse3_LDADD		= -lm
insn_ssse3_SOURCES	= insn_ssse3.def
insn_ssse3_LDADD	= -lm
insn_fpu_SOURCES	= insn_fpu.def
insn_fpu_LDADD		= -lm
insn_pclmulqdq_SOURCES  = insn_pclmulqdq.def
fxtract_LDADD		= -lm
fma4_CFLAGS             = $(AM_CFLAGS)
if VGCONF_OS_IS_SOLARIS
fma4_CFLAGS		+= -D__EXTENSIONS__
endif
fma4_LDADD		= -lm
jrcxz_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
looper_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
sbbmisc_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@
shrld_CFLAGS		= $(AM_CFLAGS) @FLAG_NO_PIE@

.def.c:
	$(PERL) $(srcdir)/gen_insn_test.pl < $< > $@
