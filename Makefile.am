
AUTOMAKE_OPTIONS = 1.10

include $(top_srcdir)/Makefile.all.am 

TOOLS = \
		memcheck \
		cachegrind \
		callgrind \
		helgrind \
		drd \
		massif \
		dhat \
		lackey \
		none

EXP_TOOLS = \
		exp-bbv

# Put docs last because building the HTML is slow and we want to get
# everything else working before we try it.
SUBDIRS = \
	include \
	VEX \
	coregrind \
	. \
	$(TOOLS) \
	$(EXP_TOOLS) \
	shared \
	tests \
	perf \
	gdbserver_tests \
	memcheck/tests/vbit-test \
	auxprogs \
	mpi \
	solaris \
	docs
DIST_SUBDIRS  = $(SUBDIRS)

SUPP_FILES = \
	glibc-2.2.supp glibc-2.3.supp glibc-2.4.supp glibc-2.5.supp \
	glibc-2.6.supp glibc-2.7.supp glibc-2.X.supp.in \
	xfree-3.supp xfree-4.supp \
	glibc-2.X-helgrind.supp \
	glibc-2.2-LinuxThreads-helgrind.supp \
	glibc-2.X-drd.supp \
	darwin9.supp darwin9-drd.supp \
	freebsd.supp freebsd-helgrind.supp freebsd-drd.supp \
	darwin10.supp darwin10-drd.supp \
	darwin11.supp darwin12.supp darwin13.supp darwin14.supp darwin15.supp \
	darwin16.supp darwin17.supp \
	bionic.supp \
	solaris11.supp solaris12.supp \
	musl.supp
DEFAULT_SUPP_FILES = @DEFAULT_SUPP@

# We include all the base .supp files in the distribution, but not
# default.supp, as it is built from the base .supp files at compile-time.
dist_noinst_DATA = $(SUPP_FILES)

vglibdir = $(pkglibexecdir)
vglib_DATA = default.supp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = valgrind.pc

BUILT_SOURCES  = default.supp include/vgversion.h valgrind.pc
CLEANFILES     = default.supp

default.supp: $(DEFAULT_SUPP_FILES)
	echo "# This is a generated file, composed of the following suppression rules:" > default.supp
	echo "# " $(DEFAULT_SUPP_FILES) >> default.supp
	cat $^ >> default.supp

## Preprend @PERL@ because tests/vg_regtest isn't executable
## Ensure make exits with error if PERL fails or post_regtest_checks fails.
regtest: check
	$(top_srcdir)/gdbserver_tests/make_local_links $(GDB)
	if @PERL@ tests/vg_regtest gdbserver_tests $(TOOLS) $(EXP_TOOLS) ; then \
	   tests/post_regtest_checks $(abs_top_srcdir) gdbserver_tests $(TOOLS) $(EXP_TOOLS); \
	else \
	   tests/post_regtest_checks $(abs_top_srcdir) gdbserver_tests $(TOOLS) $(EXP_TOOLS); \
	   false; \
	fi
post-regtest-checks:
	tests/post_regtest_checks $(abs_top_srcdir) gdbserver_tests $(TOOLS) $(EXP_TOOLS)
nonexp-regtest: check
	@PERL@ tests/vg_regtest $(TOOLS)
exp-regtest: check
	@PERL@ tests/vg_regtest gdbserver_tests $(EXP_TOOLS)
# Nb: gdbserver_tests are put in exp-regtest rather than nonexp-regtest
# because they are tested with various valgrind tools, so might be using
# an experimental tool.

## Preprend @PERL@ because tests/vg_perf isn't executable
perf: check
	@PERL@ perf/vg_perf perf

# Auxiliary test suites run under valgrind
auxchecks: all
	$(MAKE) -C auxprogs auxchecks

# Nb: no need to include any Makefile.am files here, or files included from
# them, as automake includes them automatically.  Also not COPYING, README
# or NEWS.
# We include valgrind.spec as well as valgrind.spec.in to save packagers
# from having to run configure (bug 188560).
EXTRA_DIST = \
	COPYING.DOCS \
	README_DEVELOPERS \
	README_DEVELOPERS_processes \
	README_PACKAGERS \
	README_MISSING_SYSCALL_OR_IOCTL \
	README.s390 \
	README.android \
	README.android_emulator \
	README.mips \
	README.aarch64 \
	README.solaris \
	README.freebsd \
	NEWS.old \
	valgrind.pc.in \
	valgrind.spec.in \
	valgrind.spec \
	autogen.sh

dist-hook: include/vgversion.h
	cp -p include/vgversion.h $(distdir)/include/vgversion_dist.h

dist_noinst_SCRIPTS = \
	vg-in-place

all-local: default.supp include/vgversion.h
	mkdir -p $(inplacedir)
	rm -f $(inplacedir)/default.supp
	ln -s ../default.supp $(inplacedir)

clean-local:
	rm -rf $(inplacedir) include/vgversion.h

# Need config.h in the installed tree, since some files depend on it
pkginclude_HEADERS = config.h

# vgversion.h defines accurate versions to report with -v --version
include/vgversion.h:
	$(top_srcdir)/auxprogs/make_or_upd_vgversion_h $(top_srcdir)

.PHONY: include/vgversion.h

