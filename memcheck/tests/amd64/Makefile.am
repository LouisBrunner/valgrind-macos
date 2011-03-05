
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
	fxsave-amd64.vgtest fxsave-amd64.stdout.exp fxsave-amd64.stderr.exp \
	more_x87_fp.stderr.exp more_x87_fp.stdout.exp more_x87_fp.vgtest \
	sse_memory.stderr.exp sse_memory.stdout.exp sse_memory.vgtest \
	xor-undef-amd64.stderr.exp xor-undef-amd64.stdout.exp \
	xor-undef-amd64.vgtest

check_PROGRAMS = \
	bt_everything \
	bug132146 \
	fxsave-amd64 \
	more_x87_fp \
	sse_memory \
	xor-undef-amd64

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

more_x87_fp_CFLAGS	= $(AM_CFLAGS) -O -ffast-math -mfpmath=387 \
				-mfancy-math-387
more_x87_fp_LDADD	= -lm

