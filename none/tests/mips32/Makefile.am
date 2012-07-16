
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	branches.stdout.exp branches.stderr.exp branches.vgtest \
	FPUarithmetic.stdout.exp FPUarithmetic.stdout.exp-mips32 \
        FPUarithmetic.stderr.exp FPUarithmetic.vgtest \
	LoadStore.stdout.exp LoadStore.stdout.exp-BE LoadStore.stderr.exp \
	LoadStore.vgtest \
	LoadStore1.stdout.exp LoadStore1.stdout.exp-LE LoadStore1.stderr.exp \
	LoadStore1.vgtest \
	MemCpyTest.stdout.exp MemCpyTest.stderr.exp MemCpyTest.vgtest \
	MIPS32int.stdout.exp MIPS32int.stdout.exp-BE MIPS32int.stdout.exp-mips32 \
        MIPS32int.stderr.exp MIPS32int.vgtest \
	MoveIns.stdout.exp MoveIns.stdout.exp-BE MoveIns.stderr.exp MoveIns.vgtest \
	round.stdout.exp round.stderr.exp round.vgtest \
	vfp.stdout.exp vfp.stdout.exp-BE vfp.stdout.exp-mips32 vfp.stderr.exp \
        vfp.vgtest

check_PROGRAMS = \
	allexec \
	branches \
	FPUarithmetic \
	LoadStore \
	LoadStore1 \
	MemCpyTest \
	MIPS32int \
	MoveIns \
	round \
	vfp

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

allexec_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
