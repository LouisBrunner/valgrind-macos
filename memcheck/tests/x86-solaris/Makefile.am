
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	scalar.stderr.exp scalar.stdout.exp scalar.vgtest \
	scalar_obsolete.stderr.exp scalar_obsolete.stdout.exp scalar_obsolete.vgtest \
	context_eflags.stderr.exp context_eflags.stdout.exp context_eflags.vgtest \
	context_eflags2.stderr.exp context_eflags2.stdout.exp context_eflags2.vgtest \
	context_fpu.stderr.exp context_fpu.stdout.exp context_fpu.vgtest \
	context_gpr.stderr.exp context_gpr.stdout.exp context_gpr.vgtest \
	context_sse.stderr.exp context_sse.stdout.exp context_sse.vgtest \
	ldsoexec.stderr.exp ldsoexec.vgtest

check_PROGRAMS = \
	scalar \
	context_eflags \
	context_eflags2 \
	context_fpu \
	context_gpr \
	context_sse \
	ldsoexec

if SOLARIS_OLD_SYSCALLS
check_PROGRAMS += scalar_obsolete
endif

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

scalar_CFLAGS = $(AM_CFLAGS) -I../solaris
