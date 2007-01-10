noinst_SCRIPTS = filter_stderr

noinst_HEADERS = 

INSN_TESTS = insn_basic insn_mmx insn_sse insn_sse2 insn_fpu

EXTRA_DIST = $(noinst_SCRIPTS) \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS)) \
	bt_everything.stderr.exp bt_everything.stdout.exp \
		bt_everything.vgtest \
	bug132146.vgtest bug132146.stderr.exp bug132146.stdout.exp \
	fxsave-amd64.vgtest fxsave-amd64.stdout.exp fxsave-amd64.stderr.exp \
	more_x87_fp.stderr.exp more_x87_fp.stdout.exp more_x87_fp.vgtest \
	sse_memory.stderr.exp sse_memory.stdout.exp sse_memory.vgtest

check_PROGRAMS = bt_everything bug132146 fxsave-amd64 more_x87_fp sse_memory

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
		@FLAG_M64@
AM_CXXFLAGS = $(AM_CFLAGS)

more_x87_fp_CFLAGS	= $(AM_CFLAGS) -O -ffast-math -mfpmath=387 \
				-mfancy-math-387
more_x87_fp_LDADD	= -lm
