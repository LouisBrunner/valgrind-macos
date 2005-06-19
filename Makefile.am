
AUTOMAKE_OPTIONS = foreign 1.6 dist-bzip2

include $(top_srcdir)/Makefile.all.am 

## addrcheck must come after memcheck, for mac_*.o
TOOLS =		memcheck \
		cachegrind \
		corecheck \
		massif \
		lackey \
		none

##		addrcheck \

# Temporary: we want to compile Helgrind, but not regtest it.
# Put docs last because building the HTML is slow and we want to get
# everything else working before we try it.
SUBDIRS = 	include coregrind . tests auxprogs $(TOOLS) helgrind docs
##DIST_SUBDIRS =	$(SUBDIRS) helgrind

SUPP_FILES = \
	glibc-2.1.supp glibc-2.2.supp glibc-2.3.supp \
	xfree-3.supp xfree-4.supp

dist_val_DATA = $(SUPP_FILES) default.supp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = valgrind.pc

BUILT_SOURCES = default.supp valgrind.pc

DISTCLEANFILES = default.supp

default.supp: $(SUPP_FILES)

## Preprend @PERL@ because tests/vg_regtest isn't executable
regtest: check
	@PERL@ tests/vg_regtest $(TOOLS)

EXTRA_DIST = \
	FAQ.txt \
	ACKNOWLEDGEMENTS \
	README_DEVELOPERS \
	README_PACKAGERS \
	README_XML_OUTPUT.txt \
	THREADS_SYSCALLS_SIGNALS.txt \
	README_MISSING_SYSCALL_OR_IOCTL TODO NOTES.txt \
	valgrind.spec valgrind.spec.in valgrind.pc.in \
	Makefile.all.am Makefile.tool.am Makefile.core-AM_CPPFLAGS.am \
	Makefile.tool-inplace.am

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(valdir)

all-local:
	mkdir -p $(inplacedir)
	rm -f $(addprefix $(inplacedir)/,default.supp $(SUPP_FILES))
	ln -s ../default.supp $(inplacedir)
	ln -s $(addprefix ../$(top_srcdir)/,$(SUPP_FILES)) $(inplacedir)

distclean-local:
	rm -rf $(inplacedir)
