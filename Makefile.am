
SUBDIRS = 	coregrind . docs tests include auxprogs \
		addrcheck \
		memcheck \
		cachegrind \
		corecheck \
		helgrind \
		lackey \
		none

AM_CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

SUPP_FILES = glibc-2.1.supp glibc-2.2.supp xfree-3.supp xfree-4.supp

val_DATA = $(SUPP_FILES) default.supp

BUILT_SOURCES = default.supp

DISTCLEANFILES = default.supp

default.supp: $(SUPP_FILES)

bzdist: dist
	gunzip -c $(PACKAGE)-$(VERSION).tar.gz | bzip2 > $(PACKAGE)-$(VERSION).tar.bz2

EXTRA_DIST = $(val_DATA) \
	PATCHES_APPLIED ACKNOWLEDGEMENTS \
	README_KDE3_FOLKS README_PACKAGERS \
	README_MISSING_SYSCALL_OR_IOCTL TODO \
	valgrind.spec valgrind.spec.in

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(valdir)
	rm -f $(DESTDIR)$(valdir)/libpthread.so.0
	$(LN_S) libpthread.so $(DESTDIR)$(valdir)/libpthread.so.0

