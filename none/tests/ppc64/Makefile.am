
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	jm-int.stderr.exp jm-int.stdout.exp jm-int.vgtest \
	jm-fp.stderr.exp  jm-fp.stdout.exp  jm-fp.vgtest \
	jm-vmx.stderr.exp jm-vmx.stdout.exp jm-vmx.stdout.exp_Minus_nan \
	jm-vmx.vgtest \
	lsw.stderr.exp lsw.stdout.exp lsw.vgtest \
	std_reg_imm.vgtest std_reg_imm.stderr.exp std_reg_imm.stdout.exp \
	round.stderr.exp round.stdout.exp round.vgtest \
	twi_tdi.stderr.exp twi_tdi.stdout.exp twi_tdi.vgtest \
	tw_td.stderr.exp tw_td.stdout.exp tw_td.vgtest \
	power6_bcmp.stderr.exp power6_bcmp.stdout.exp power6_bcmp.vgtest \
	power6_mf_gpr.stderr.exp power6_mf_gpr.stdout.exp power6_mf_gpr.vgtest \
	test_isa_2_06_part1.stderr.exp  test_isa_2_06_part1.stdout.exp  test_isa_2_06_part1.vgtest \
	test_isa_2_06_part2.stderr.exp  test_isa_2_06_part2.stdout.exp  test_isa_2_06_part2.vgtest \
	test_isa_2_06_part3.stderr.exp  test_isa_2_06_part3.stdout.exp  test_isa_2_06_part3.vgtest \
	test_dfp1.stderr.exp test_dfp1.stdout.exp test_dfp1.vgtest \
	test_dfp2.stderr.exp test_dfp2.stdout.exp test_dfp2.vgtest \
	test_dfp2.stdout.exp_Without_dcffix \
	test_dfp3.stderr.exp test_dfp3.stdout.exp test_dfp3.vgtest \
	test_dfp4.stderr.exp test_dfp4.stdout.exp test_dfp4.vgtest \
	test_dfp5.stderr.exp test_dfp5.stdout.exp test_dfp5.vgtest

check_PROGRAMS = \
	allexec \
	jm-insns lsw round std_reg_imm twi_tdi tw_td power6_bcmp power6_mf_gpr test_isa_2_06_part1 \
	test_isa_2_06_part2 test_isa_2_06_part3 test_dfp1 test_dfp2 test_dfp3 test_dfp4 test_dfp5

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS		= $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

if HAS_ALTIVEC
ALTIVEC_FLAG = -DHAS_ALTIVEC
else
ALTIVEC_FLAG =
endif

if HAS_VSX
BUILD_FLAG_VSX = -mvsx
VSX_FLAG = -DHAS_VSX
else
VSX_FLAG =
BUILD_FLAG_VSX =
endif

if HAS_DFP
BUILD_FLAGS_DFP = -mhard-dfp -mcpu=power6
DFP_FLAG = -DHAS_DFP
else
BUILD_FLAGS_DFP =
DFP_FLAG =
endif

test_isa_2_06_part1_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M64@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_isa_2_06_part2_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames $(VSX_FLAG) \
			@FLAG_M64@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

test_isa_2_06_part3_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(VSX_FLAG) \
			@FLAG_M64@ $(ALTIVEC_FLAG) $(BUILD_FLAG_VSX)

jm_insns_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -g -mregnames -maltivec \
			@FLAG_M64@ $(ALTIVEC_FLAG)

test_dfp1_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(DFP_FLAG) \
			@FLAG_M64@ $(BUILD_FLAGS_DFP)

test_dfp2_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(DFP_FLAG) \
			@FLAG_M64@ $(BUILD_FLAGS_DFP)
test_dfp3_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(DFP_FLAG) \
			@FLAG_M64@ $(BUILD_FLAGS_DFP)

test_dfp4_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(DFP_FLAG) \
			@FLAG_M64@ $(BUILD_FLAGS_DFP)

test_dfp5_CFLAGS = $(AM_CFLAGS) -Winline -Wall -O -lm -g -mregnames $(DFP_FLAG) \
			@FLAG_M64@ $(BUILD_FLAGS_DFP)

