
AUTOMAKE_OPTIONS = foreign 1.6 dist-bzip2

## include must be first for vg_skin.h
## addrcheck must come after memcheck, for mac_*.o
SUBDIRS = 	include coregrind . docs tests auxprogs \
		memcheck \
		addrcheck \
		cachegrind \
		corecheck \
		helgrind \
		lackey \
		none

AM_CPPFLAGS = -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

SUPP_FILES = \
	glibc-2.1.supp glibc-2.2.supp glibc-2.3.supp \
	xfree-3.supp xfree-4.supp

val_DATA = $(SUPP_FILES) default.supp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = valgrind.pc

BUILT_SOURCES = default.supp valgrind.pc

DISTCLEANFILES = default.supp

default.supp: $(SUPP_FILES)

## Preprend @PERL@ because tests/vg_regtest isn't executable
regtest: check
	@PERL@ tests/vg_regtest --all

EXTRA_DIST = $(val_DATA) \
	FAQ.txt \
	ACKNOWLEDGEMENTS \
	README_PACKAGERS \
	README_MISSING_SYSCALL_OR_IOCTL TODO \
	valgrind.spec valgrind.spec.in valgrind.pc.in

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(valdir)
	rm -f $(DESTDIR)$(valdir)/libpthread.so.0
	$(LN_S) libpthread.so $(DESTDIR)$(valdir)/libpthread.so.0

all-local:
	mkdir -p .in_place
	rm -f $(addprefix .in_place/,default.supp $(SUPP_FILES))
	ln -s $(addprefix ../,default.supp $(SUPP_FILES)) $(top_srcdir)/.in_place

