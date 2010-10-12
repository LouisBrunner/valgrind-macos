
AUTOMAKE_OPTIONS = foreign 1.7 dist-bzip2

include $(top_srcdir)/Makefile.all.am 

TOOLS =		memcheck \
		cachegrind \
		callgrind \
		massif \
		lackey \
		none \
		helgrind \
		drd

EXP_TOOLS = 	exp-ptrcheck \
		exp-bbv \
		exp-dhat

# DDD: once all tools work on Darwin, TEST_TOOLS and TEST_EXP_TOOLS can be
# replaced with TOOLS and EXP_TOOLS.
TEST_TOOLS = $(TOOLS)
if !VGCONF_OS_IS_DARWIN
  TEST_EXP_TOOLS = $(EXP_TOOLS)
else
  TEST_EXP_TOOLS = exp-bbv
endif

# Put docs last because building the HTML is slow and we want to get
# everything else working before we try it.
SUBDIRS = \
	include \
	VEX \
	coregrind \
	. \
	$(TOOLS) \
	$(EXP_TOOLS) \
	tests \
	perf \
	auxprogs \
	mpi \
	docs
DIST_SUBDIRS  = $(SUBDIRS)

SUPP_FILES = \
	glibc-2.2.supp glibc-2.3.supp glibc-2.4.supp glibc-2.5.supp \
	glibc-2.6.supp glibc-2.7.supp glibc-2.X.supp.in \
	aix5libc.supp xfree-3.supp xfree-4.supp \
	glibc-2.34567-NPTL-helgrind.supp \
	glibc-2.2-LinuxThreads-helgrind.supp \
	glibc-2.X-drd.supp \
	exp-ptrcheck.supp \
	darwin9.supp darwin9-drd.supp \
	darwin10.supp darwin10-drd.supp
DEFAULT_SUPP_FILES = @DEFAULT_SUPP@

# We include all the base .supp files in the distribution, but not
# default.supp, as it is built from the base .supp files at compile-time.
dist_noinst_DATA = $(SUPP_FILES)

pkglib_DATA = default.supp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = valgrind.pc

BUILT_SOURCES  = default.supp valgrind.pc
CLEANFILES     = default.supp

default.supp: $(DEFAULT_SUPP_FILES)
	echo "# This is a generated file, composed of the following suppression rules:" > default.supp
	echo "# " $(DEFAULT_SUPP_FILES) >> default.supp
	cat $(DEFAULT_SUPP_FILES) >> default.supp

## Preprend @PERL@ because tests/vg_regtest isn't executable
regtest: check
	@PERL@ tests/vg_regtest $(TEST_TOOLS) $(TEST_EXP_TOOLS)
nonexp-regtest: check
	@PERL@ tests/vg_regtest $(TEST_TOOLS)
exp-regtest: check
	@PERL@ tests/vg_regtest $(TEST_EXP_TOOLS)

## Preprend @PERL@ because tests/vg_perf isn't executable
perf: check
	@PERL@ perf/vg_perf perf

# Nb: no need to include any Makefile.am files here, or files included from
# them, as automake includes them automatically.  Also not COPYING, README
# or NEWS.
# We include valgrind.spec as well as valgrind.spec.in to save packagers
# from having to run configure (bug 188560).
EXTRA_DIST = \
	COPYING.DOCS \
	README_DEVELOPERS \
	README_PACKAGERS \
	README_MISSING_SYSCALL_OR_IOCTL \
	valgrind.pc.in \
	valgrind.spec.in \
	valgrind.spec

dist_noinst_SCRIPTS = \
	vg-in-place

all-local: default.supp
	mkdir -p $(inplacedir)
	rm -f $(inplacedir)/default.supp
	ln -s ../default.supp $(inplacedir)

clean-local:
	rm -rf $(inplacedir)


