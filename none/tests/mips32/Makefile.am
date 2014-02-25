
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	block_size.stdout.exp block_size.stderr.exp block_size.vgtest \
	branches.stdout.exp branches.stderr.exp branches.vgtest \
	FPUarithmetic.stdout.exp FPUarithmetic.stdout.exp-mips32 \
	FPUarithmetic.stderr.exp FPUarithmetic.vgtest \
	LoadStore.stdout.exp LoadStore.stdout.exp-BE LoadStore.stderr.exp \
	LoadStore.vgtest \
	LoadStore1.stdout.exp LoadStore1.stdout.exp-LE LoadStore1.stderr.exp \
	LoadStore1.vgtest \
	MemCpyTest.stdout.exp MemCpyTest.stderr.exp MemCpyTest.vgtest \
	MIPS32int.stdout.exp-mips32-BE MIPS32int.stdout.exp-mips32r2-BE \
	MIPS32int.stdout.exp-mips32-LE MIPS32int.stdout.exp-mips32r2-LE \
	MIPS32int.stderr.exp MIPS32int.vgtest \
	MoveIns.stdout.exp MoveIns.stdout.exp-BE MoveIns.stderr.exp MoveIns.vgtest \
	round.stdout.exp round.stderr.exp round.vgtest \
	vfp.stdout.exp-mips32-BE vfp.stdout.exp-mips32r2-BE \
	vfp.stdout.exp-mips32-LE vfp.stdout.exp-mips32r2-LE vfp.stderr.exp \
	vfp.vgtest \
	SignalException.stderr.exp SignalException.vgtest \
	bug320057-mips32.stdout.exp bug320057-mips32.stderr.exp \
	bug320057-mips32.vgtest \
	mips32_dsp.stdout.exp-LE mips32_dsp.stdout.exp-BE \
	mips32_dsp.stderr.exp mips32_dsp.vgtest \
	mips32_dspr2.stdout.exp mips32_dspr2.stderr.exp \
	mips32_dspr2.vgtest \
	unaligned_load_store.stdout.exp-LE unaligned_load_store.stdout.exp-BE \
	unaligned_load_store.stderr.exp unaligned_load_store.vgtest \
	test_fcsr.stdout.exp test_fcsr.stderr.exp test_fcsr.vgtest \
	test_math.stdout.exp test_math.stderr.exp test_math.vgtest \
	round_fpu64.stdout.exp round_fpu64.stdout.exp-fpu32 \
	round_fpu64.stderr.exp round_fpu64.vgtest \
	fpu_branches.stdout.exp fpu_branches.stderr.exp fpu_branches.vgtest

check_PROGRAMS = \
	allexec \
	block_size \
	branches \
	FPUarithmetic \
	LoadStore \
	LoadStore1 \
	MemCpyTest \
	MIPS32int \
	MoveIns \
	round \
	vfp \
	SignalException \
	bug320057-mips32 \
	mips32_dsp \
	mips32_dspr2 \
	unaligned_load_store \
	test_fcsr \
	test_math \
	round_fpu64 \
	fpu_branches

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

allexec_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
bug320057_mips32_LDFLAGS = -lrt

# C++ tests
test_math_SOURCES          = test_math.cpp
