

SUBDIRS = demangle . docs tests

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

#LDFLAGS = -Wl,-z -Wl,initfirst

INCLUDES = -I$(srcdir)/demangle

bin_SCRIPTS = valgrind vg_annotate

SUPP_FILES = glibc-2.1.supp glibc-2.2.supp xfree-3.supp xfree-4.supp

val_DATA = $(SUPP_FILES) default.supp

BUILT_SOURCES = default.supp

default.supp: $(SUPP_FILES)

bzdist: dist
	gunzip -c $(PACKAGE)-$(VERSION).tar.gz | bzip2 > $(PACKAGE)-$(VERSION).tar.bz2

EXTRA_DIST = $(val_DATA) \
	PATCHES_APPLIED ACKNOWLEDGEMENTS \
	README_KDE3_FOLKS README_PACKAGERS \
	README_MISSING_SYSCALL_OR_IOCTL TODO dosyms vg_libpthread.vs \
	valgrind.spec valgrind.spec.in \
	vg_profile.c \
	vg_cachesim_I1.c vg_cachesim_D1.c vg_cachesim_L2.c vg_cachesim_gen.c

val_PROGRAMS = \
	valgrind.so \
	valgrinq.so \
	libpthread.so \
	vgskin_memcheck.so \
	vgskin_cachesim.so \
	vgskin_eraser.so \
	vgskin_addrcheck.so \
	vgskin_none.so \
	vgskin_lackey.so \
	vgskin_corecheck.so

libpthread_so_SOURCES = \
	vg_libpthread.c \
	vg_libpthread_unimp.c
libpthread_so_DEPENDENCIES = $(srcdir)/vg_libpthread.vs
libpthread_so_LDFLAGS	   = -Werror -fno-omit-frame-pointer -UVG_LIBDIR -shared -fpic -Wl,-version-script $(srcdir)/vg_libpthread.vs

valgrinq_so_SOURCES = vg_valgrinq_dummy.c
valgrinq_so_LDFLAGS = -shared

valgrind_so_SOURCES = \
	vg_clientfuncs.c \
	vg_scheduler.c \
	vg_clientmalloc.c \
	vg_default.c \
	vg_demangle.c \
	vg_dispatch.S \
	vg_errcontext.c \
	vg_execontext.c \
	vg_from_ucode.c \
	vg_helpers.S \
	vg_instrument.c \
	vg_main.c \
	vg_malloc2.c \
	vg_memory.c \
	vg_messages.c \
	vg_mylibc.c \
	vg_procselfmaps.c \
	vg_dummy_profile.c \
	vg_signals.c \
	vg_startup.S \
	vg_symtab2.c \
	vg_syscalls.c \
	vg_syscall.S \
	vg_to_ucode.c \
	vg_translate.c \
	vg_transtab.c
valgrind_so_LDFLAGS = -Wl,-z -Wl,initfirst -shared
valgrind_so_LDADD = \
	demangle/cp-demangle.o \
	demangle/cplus-dem.o \
	demangle/dyn-string.o \
	demangle/safe-ctype.o

vgskin_memcheck_so_SOURCES = \
	vg_memcheck.c \
	vg_memcheck_clientreqs.c \
	vg_memcheck_errcontext.c \
	vg_memcheck_from_ucode.c \
	vg_memcheck_translate.c \
	vg_memcheck_helpers.S
vgskin_memcheck_so_LDFLAGS = -shared

vgskin_cachesim_so_SOURCES = vg_cachesim.c
vgskin_cachesim_so_LDFLAGS = -shared

vgskin_eraser_so_SOURCES = vg_eraser.c
vgskin_eraser_so_LDFLAGS = -shared

vgskin_addrcheck_so_SOURCES = vg_addrcheck.c
vgskin_addrcheck_so_LDFLAGS = -shared

vgskin_none_so_SOURCES 	 = vg_none.c
vgskin_none_so_LDFLAGS   = -shared

vgskin_lackey_so_SOURCES = vg_lackey.c
vgskin_lackey_so_LDFLAGS = -shared

vgskin_corecheck_so_SOURCES = vg_corecheck.c
vgskin_corecheck_so_LDFLAGS = -shared

include_HEADERS = valgrind.h

noinst_HEADERS = \
        vg_kerneliface.h        \
        vg_include.h            \
        vg_skin.h               \
        vg_constants.h          \
        vg_constants_skin.h     \
        vg_unsafe.h		\
	vg_memcheck_include.h	\
	vg_memcheck.h

MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS) 

vg_memory.o: vg_memory.c $(MANUAL_DEPS)
	$(COMPILE) -O2 @PREFERRED_STACK_BOUNDARY@ -c $<

vg_clientfuncs.o: vg_clientfuncs.c $(MANUAL_DEPS)
	$(COMPILE) -fno-omit-frame-pointer -c $<

vg_libpthread.o: vg_libpthread.c $(MANUAL_DEPS)
	$(COMPILE) -fno-omit-frame-pointer -c $<

##valgrind.so$(EXEEXT): $(valgrind_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o valgrind.so \
##		$(valgrind_so_OBJECTS) $(valgrind_so_LDADD)

##valgrinq.so$(EXEEXT): $(valgrinq_so_OBJECTS)
##	$(CC) $(CFLAGS) -shared -o valgrinq.so $(valgrinq_so_OBJECTS)

##libpthread.so$(EXEEXT): $(libpthread_so_OBJECTS) $(srcdir)/vg_libpthread.vs
##	$(CC) -Wall -Werror -g -O -shared -fpic -o libpthread.so \
##		$(libpthread_so_OBJECTS) \
##		-Wl,-version-script $(srcdir)/vg_libpthread.vs

##vgskin_memcheck.so$(EXEEXT): $(vgskin_memcheck_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_memcheck.so \
##		$(vgskin_memcheck_so_OBJECTS)

##vgskin_cachesim.so$(EXEEXT): $(vgskin_cachesim_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_cachesim.so \
##		$(vgskin_cachesim_so_OBJECTS)

##vgskin_eraser.so$(EXEEXT): $(vgskin_eraser_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_eraser.so \
##		$(vgskin_eraser_so_OBJECTS)

##vgskin_none.so$(EXEEXT): $(vgskin_none_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_none.so \
##		$(vgskin_none_so_OBJECTS)

##vgskin_lackey.so$(EXEEXT): $(vgskin_lackey_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_lackey.so \
##		$(vgskin_lackey_so_OBJECTS)

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(valdir)
	rm -f $(DESTDIR)$(valdir)/libpthread.so.0
	$(LN_S) libpthread.so $(DESTDIR)$(valdir)/libpthread.so.0

