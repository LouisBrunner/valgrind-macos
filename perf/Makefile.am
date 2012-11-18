
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = vg_perf

EXTRA_DIST = \
	bigcode1.vgperf \
	bigcode2.vgperf \
	bz2.vgperf \
	fbench.vgperf \
	ffbench.vgperf \
	heap.vgperf \
	heap_pdb4.vgperf \
	many-loss-records.vgperf \
	many-xpts.vgperf \
	sarp.vgperf \
	tinycc.vgperf \
	test_input_for_tinycc.c

check_PROGRAMS = \
	bigcode bz2 fbench ffbench heap many-loss-records many-xpts sarp tinycc

AM_CFLAGS   += -O $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += -O $(AM_FLAG_M3264_PRI)


# Extra stuff
bz2_CFLAGS	= $(AM_CFLAGS) -Wno-inline

fbench_CFLAGS   = $(AM_CFLAGS) -O2
ffbench_LDADD	= -lm

tinycc_CFLAGS	= $(AM_CFLAGS) -Wno-shadow -Wno-inline
if HAS_POINTER_SIGN_WARNING
tinycc_CFLAGS  += -Wno-pointer-sign
endif
