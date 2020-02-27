
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	block_size.stdout.exp block_size.stderr.exp block_size.vgtest \
	branch_pc.stdout.exp branch_pc.stderr.exp branch_pc.vgtest \
	branches.stdout.exp branches.stderr.exp branches.vgtest \
	branches.stdout.exp-r6 \
	branches_r6.stdout.exp branches_r6.stderr.exp branches_r6.vgtest \
	bug320057-mips32.stdout.exp bug320057-mips32.stderr.exp \
	bug320057-mips32.vgtest \
	change_fp_mode.stdout.exp change_fp_mode.stdout.exp-fpu32 \
	change_fp_mode.stderr.exp change_fp_mode.vgtest \
	change_fp_mode.stdout.exp-r6 \
	FPUarithmetic.stdout.exp FPUarithmetic.stdout.exp-mips32 \
	FPUarithmetic.stderr.exp FPUarithmetic.vgtest \
	fpu_branches.stdout.exp fpu_branches.stderr.exp fpu_branches.vgtest \
	LoadStore.stdout.exp LoadStore.stdout.exp-BE LoadStore.stderr.exp \
	LoadStore.vgtest LoadStore.stdout.exp-r6-LE LoadStore.stdout.exp-r6-BE \
	LoadStore1.stdout.exp LoadStore1.stdout.exp-LE LoadStore1.stderr.exp \
	LoadStore1.vgtest LoadStore1.stdout.exp-r6-LE LoadStore1.stdout.exp-r6-BE\
	MemCpyTest.stdout.exp MemCpyTest.stderr.exp MemCpyTest.vgtest \
	MIPS32int.stdout.exp-mips32-BE MIPS32int.stdout.exp-mips32r2-BE \
	MIPS32int.stdout.exp-mips32-LE MIPS32int.stdout.exp-mips32r2-LE \
	MIPS32int.stderr.exp MIPS32int.vgtest \
	MIPS32int.stdout.exp-mips32r6-LE MIPS32int.stdout.exp-mips32r6-BE \
	MIPS32r6int.stdout.exp MIPS32r6int.stderr.exp MIPS32r6int.vgtest \
	mips32_dsp.stdout.exp-LE mips32_dsp.stdout.exp-BE \
	mips32_dsp.stderr.exp mips32_dsp.vgtest \
	mips32_dspr2.stdout.exp mips32_dspr2.stderr.exp \
	mips32_dspr2.vgtest \
	MoveIns.stdout.exp MoveIns.stdout.exp-BE \
	MoveIns.stdout.exp-mips32r2-BE MoveIns.stdout.exp-mips32r2-LE \
	MoveIns.stderr.exp MoveIns.vgtest \
	MoveIns.stdout.exp-mips32r6-BE MoveIns.stdout.exp-mips32r6-LE \
	pc_instructions_r6.stdout.exp pc_instructions_r6.stderr.exp \
	pc_instructions_r6.vgtest \
	msa_arithmetic.stderr.exp msa_arithmetic.stdout.exp msa_arithmetic.vgtest \
	msa_comparison.stderr.exp msa_comparison.stdout.exp msa_comparison.vgtest \
	msa_data_transfer.stderr.exp msa_data_transfer.stdout.exp \
	msa_data_transfer.stdout.exp-BE msa_data_transfer.vgtest \
	msa_fpu.stderr.exp msa_fpu.stdout.exp msa_fpu.vgtest \
	msa_logical_and_shift.stderr.exp msa_logical_and_shift.stdout.exp \
	msa_logical_and_shift.vgtest msa_shuffle.stderr.exp msa_shuffle.stdout.exp \
	msa_shuffle.vgtest \
	round_fpu64.stdout.exp round_fpu64.stdout.exp-fpu32 \
	round_fpu64.stderr.exp round_fpu64.vgtest \
	round.stdout.exp round.stderr.exp round.vgtest \
	fp_r6.stdout.exp fp_r6.stderr.exp fp_r6.vgtest \
	SignalException.stderr.exp SignalException.vgtest \
	test_fcsr.stdout.exp test_fcsr.stderr.exp test_fcsr.vgtest \
	test_math.stdout.exp test_math.stderr.exp test_math.vgtest \
	unaligned_load_store.stdout.exp-LE unaligned_load_store.stdout.exp-BE \
	unaligned_load_store.stderr.exp unaligned_load_store.vgtest \
	vfp.stdout.exp-mips32-BE vfp.stdout.exp-mips32r2-BE \
	vfp.stdout.exp-mips32-LE vfp.stdout.exp-mips32r2-LE vfp.stderr.exp \
	vfp.stdout.exp-mips32r2-fpu_64-BE vfp.stdout.exp-mips32r2-fpu_64-LE \
	vfp.vgtest


check_PROGRAMS = \
	allexec \
	block_size \
	branches \
	branch_pc \
	branches_r6 \
	change_fp_mode \
	FPUarithmetic \
	LoadStore \
	LoadStore1 \
	MemCpyTest \
	MIPS32int \
	MIPS32r6int \
	MoveIns \
	round \
	vfp \
	SignalException \
	bug320057-mips32 \
	mips32_dsp \
	mips32_dspr2 \
	unaligned_load_store \
	pc_instructions_r6 \
	test_fcsr \
	test_math \
	round_fpu64 \
	fp_r6 \
	fpu_branches \
	msa_arithmetic \
	msa_comparison \
	msa_data_transfer \
	msa_fpu \
	msa_logical_and_shift \
	msa_shuffle \
	unaligned_load_store \
	test_fcsr \
	test_math \
	round_fpu64 \
	fpu_branches

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

allexec_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

if !VGCONF_HAVE_PLATFORM_SEC
msa_arithmetic_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_comparison_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_data_transfer_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_fpu_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@ -lm
msa_logical_and_shift_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
msa_shuffle_CFLAGS = $(AM_CFLAGS) @FLAG_MSA@
endif

bug320057_mips32_LDFLAGS = -lrt

# C++ tests
test_math_SOURCES          = test_math.cpp
