
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	bug129390-ppc32.stdout.exp bug129390-ppc32.stderr.exp \
	bug129390-ppc32.vgtest \
	bug139050-ppc32.stdout.exp bug139050-ppc32.stderr.exp \
	bug139050-ppc32.vgtest \
	ldstrev.stderr.exp ldstrev.stdout.exp ldstrev.vgtest \
	jm-int-sh_algebraic.stderr.exp jm-int-sh_algebraic.stdout.exp \
	jm-int-sh_algebraic.vgtest \
	jm-int.stderr.exp jm-int.stdout.exp jm-int.vgtest \
	jm-int_other.stderr.exp jm-int_other.stdout.exp jm-int_other.vgtest \
	jm-fp.stderr.exp jm-fp.stdout.exp jm-fp.vgtest jm-fp.stdout.exp-BE2 \
	jm-vmx.stderr.exp jm-vmx.stdout.exp jm-vmx.stdout.exp_Minus_nan \
	jm-vmx.vgtest \
	jm-mfspr.stderr.exp jm-mfspr.stdout.exp jm-mfspr.vgtest \
	jm-misc.stderr.exp jm-misc.stdout.exp jm-misc.vgtest \
	lsw.stderr.exp lsw.stdout.exp lsw.vgtest \
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
	opcodes.h \
	power5+_round.stderr.exp power5+_round.stdout.exp power5+_round.vgtest \
	power6_bcmp.stderr.exp power6_bcmp.stdout.exp power6_bcmp.vgtest \
	test_isa_2_06_part1.stderr.exp  test_isa_2_06_part1.stdout.exp  test_isa_2_06_part1.vgtest \
	test_isa_2_06_part2.stderr.exp  test_isa_2_06_part2.stdout.exp  test_isa_2_06_part2.vgtest \
	test_isa_2_06_part3.stderr.exp  test_isa_2_06_part3.stdout.exp  test_isa_2_06_part3.vgtest \
	test_isa_2_06_part2-div.stderr.exp  test_isa_2_06_part2-div.stdout.exp  test_isa_2_06_part2-div.vgtest \
	test_isa_2_06_part3-div.stderr.exp  test_isa_2_06_part3-div.stdout.exp  test_isa_2_06_part3-div.vgtest \
	test_dfp1.stderr.exp test_dfp1.stdout.exp test_dfp1.vgtest \
	test_dfp2.stderr.exp test_dfp2.stdout.exp test_dfp2.vgtest \
	test_dfp2.stdout.exp_Without_dcffix \
	test_dfp3.stderr.exp test_dfp3.stdout.exp test_dfp3.vgtest \
	test_dfp4.stderr.exp test_dfp4.stdout.exp test_dfp4.vgtest \
	test_dfp5.stderr.exp test_dfp5.stdout.exp test_dfp5.vgtest \
	jm_vec_isa_2_07.stderr.exp jm_vec_isa_2_07.vgtest \
	jm_vec_isa_2_07.stdout.exp jm_vec_isa_2_07.stdout.exp-LE \
	jm_fp_isa_2_07.stderr.exp jm_fp_isa_2_07.stdout.exp jm_fp_isa_2_07.vgtest \
	jm_int_isa_2_07.stderr.exp jm_int_isa_2_07.vgtest \
	jm_int_isa_2_07.stdout.exp \
	test_isa_2_07_part2.stderr.exp test_isa_2_07_part2.stdout.exp test_isa_2_07_part2.vgtest \
	test_tm.stderr.exp test_tm.stdout.exp test_tm.vgtest \
	test_touch_tm.stderr.exp test_touch_tm.stdout.exp test_touch_tm.vgtest \
	ldst_multiple.stderr.exp ldst_multiple.stdout.exp ldst_multiple.vgtest \
	data-cache-instructions.stderr.exp data-cache-instructions.stdout.exp data-cache-instructions.vgtest

check_PROGRAMS = \
	allexec \
	lsw jm-insns round \
	test_isa_2_06_part1 test_isa_2_06_part2 test_isa_2_06_part3 \
	test_dfp1 test_dfp2 test_dfp3 test_dfp4 test_dfp5 \
	test_isa_2_07_part1 test_isa_2_07_part2 \
	test_tm test_touch_tm ldst_multiple data-cache-instructions \
	test_fx test_gx \
	testVMX twi tw xlc_dbl_u32 power5+_round power6_bcmp \
	bug129390-ppc32 bug139050-ppc32 \
	ldstrev mftocrf mcrfs

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

allexec_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

if HAS_ALTIVEC
BUILD_FLAG_ALTIVEC = -maltivec
BUILD_FLAG_ABI_ALTIVEC = -mabi=altivec
ALTIVEC_FLAG = -DHAS_ALTIVEC
else
BUILD_FLAG_ALTIVEC =
BUILD_FLAG_ABI_ALTIVEC =
ALTIVEC_FLAG =
endif

if HAS_VSX
BUILD_FLAG_VSX = -mvsx
VSX_FLAG = -DHAS_VSX
else
BUILD_FLAG_VSX =
VSX_FLAG =
endif

if HAS_DFP
BUILD_FLAGS_DFP = -mhard-dfp -mcpu=power6
DFP_FLAG = -DHAS_DFP
else
BUILD_FLAGS_DFP =
DFP_FLAG =
endif

if HAS_ISA_2_07
BUILD_FLAGS_ISA_2_07 = -mcpu=power8
ISA_2_07_FLAG = -DHAS_ISA_2_07
else
BUILD_FLAGS_ISA_2_07 =
ISA_2_07_FLAG =
endif

if SUPPORTS_HTM
HTM_FLAG = -mhtm -DSUPPORTS_HTM
endif

jm_insns_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_ALTIVEC)

testVMX_CFLAGS  = $(AM_CFLAGS) -O -g -Wall -DALTIVEC \
			-DGCC_COMPILER @FLAG_M32@  $(BUILD_FLAG_ALTIVEC) \
			$(BUILD_FLAG_ABI_ALTIVEC)

test_isa_2_06_part1_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_isa_2_06_part2_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_isa_2_06_part3_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M32@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_dfp1_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(DFP_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_DFP)
test_dfp2_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(DFP_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_DFP)
test_dfp3_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(DFP_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_DFP)
test_dfp4_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(DFP_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_DFP)
test_dfp5_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(DFP_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_DFP)

test_isa_2_07_part1_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(ISA_2_07_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_ISA_2_07)
test_isa_2_07_part2_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(ISA_2_07_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_ISA_2_07)

test_tm_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(HTM_FLAG) $(ISA_2_07_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_ISA_2_07)
test_touch_tm_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(HTM_FLAG) $(ISA_2_07_FLAG) \
			@FLAG_M32@ $(BUILD_FLAGS_ISA_2_07)

test_isa_2_06_part3_LDADD = -lm
test_dfp1_LDADD = -lm
test_dfp2_LDADD = -lm
test_dfp3_LDADD = -lm
test_dfp4_LDADD = -lm
test_dfp5_LDADD = -lm
test_isa_2_07_part1_LDADD = -lm
test_isa_2_07_part2_LDADD = -lm
test_tm_LDADD = -lm
test_touch_tm_LDADD = -lm

