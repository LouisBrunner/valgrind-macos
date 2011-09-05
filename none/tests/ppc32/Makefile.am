
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	bug129390-ppc32.stdout.exp bug129390-ppc32.stderr.exp \
	bug129390-ppc32.vgtest \
	bug139050-ppc32.stdout.exp bug139050-ppc32.stderr.exp \
	bug139050-ppc32.vgtest \
	ldstrev.stderr.exp ldstrev.stdout.exp ldstrev.vgtest \
	lsw.stderr.exp lsw.stdout.exp lsw.vgtest \
	jm-int.stderr.exp jm-int.stdout.exp jm-int.vgtest \
	jm-fp.stderr.exp jm-fp.stdout.exp jm-fp.vgtest \
	jm-vmx.stderr.exp jm-vmx.stdout.exp jm-vmx.stdout.exp_Minus_nan \
	jm-vmx.vgtest \
	mftocrf.stderr.exp mftocrf.stdout.exp mftocrf.vgtest \
	mcrfs.stderr.exp mcrfs.stdout.exp mcrfs.vgtest \
	round.stderr.exp round.stdout.exp round.vgtest \
	test_fx.stderr.exp test_fx.stdout.exp test_fx.stdout.exp_Minus_nan \
	test_fx.vgtest \
	test_gx.stderr.exp test_gx.stdout.exp test_gx.stdout.exp_Minus_nan \
	test_gx.vgtest \
	testVMX.stderr.exp  testVMX.stdout.exp  testVMX.vgtest \
	twi.stderr.exp twi.stdout.exp twi.vgtest \
	tw.stderr.exp tw.stdout.exp tw.vgtest \
	xlc_dbl_u32.stderr.exp xlc_dbl_u32.stdout.exp xlc_dbl_u32.vgtest \
	power5+_round.stderr.exp power5+_round.stdout.exp power5+_round.vgtest \
	power6_bcmp.stderr.exp power6_bcmp.stdout.exp power6_bcmp.vgtest \
	test_isa_2_06_part1.stderr.exp  test_isa_2_06_part1.stdout.exp  test_isa_2_06_part1.vgtest \
	test_isa_2_06_part2.stderr.exp  test_isa_2_06_part2.stdout.exp  test_isa_2_06_part2.vgtest \
	test_isa_2_06_part3.stderr.exp  test_isa_2_06_part3.stdout.exp  test_isa_2_06_part3.vgtest

check_PROGRAMS = \
	bug129390-ppc32 \
	bug139050-ppc32 \
	ldstrev lsw jm-insns mftocrf mcrfs round test_fx test_gx \
	testVMX twi tw xlc_dbl_u32 power5+_round power6_bcmp \
	test_isa_2_06_part1 \
	test_isa_2_06_part2 \
	test_isa_2_06_part3

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

if HAS_ALTIVEC
ALTIVEC_FLAG = -DHAS_ALTIVEC
else
ALTIVEC_FLAG =
endif

if HAS_VSX
BUILD_FLAG_VSX = -mvsx
VSX_FLAG = -DHAS_VSX
else
BUILD_FLAG_VSX =
VSX_FLAG =
endif

jm_insns_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames -maltivec \
			@FLAG_M32@ $(ALTIVEC_FLAG)

testVMX_CFLAGS  = $(AM_CFLAGS) -O -g -Wall -maltivec -mabi=altivec -DALTIVEC \
			-DGCC_COMPILER @FLAG_M32@

test_isa_2_06_part1_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_isa_2_06_part2_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_isa_2_06_part3_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(VSX_FLAG) \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

