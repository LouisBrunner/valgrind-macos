
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	context_fpu.stderr.exp context_fpu.stdout.exp context_fpu.vgtest \
	context_gpr.stderr.exp context_gpr.stdout.exp context_gpr.vgtest \
	context_rflags.stderr.exp context_rflags.stdout.exp context_rflags.vgtest \
	context_rflags2.stderr.exp context_rflags2.stdout.exp context_rflags2.vgtest \
	context_sse.stderr.exp context_sse.stdout.exp context_sse.vgtest \
	ldsoexec.stderr.exp ldsoexec.vgtest \
	scalar.stderr.exp scalar.stdout.exp scalar.vgtest

check_PROGRAMS = \
	context_fpu \
	context_gpr \
	context_rflags \
	context_rflags2 \
	context_sse \
	ldsoexec \
	scalar

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

scalar_CFLAGS = $(AM_CFLAGS) -I../solaris
