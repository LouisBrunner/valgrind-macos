SUBDIRS = demangle . docs tests

valdir = $(libdir)/valgrind

LDFLAGS = -Wl,-z -Wl,initfirst

INCLUDES = -I$(srcdir)/demangle

bin_SCRIPTS = valgrind

SUPP_FILES = glibc-2.1.supp glibc-2.2.supp xfree-3.supp xfree-4.supp

val_DATA = $(SUPP_FILES) default.supp

BUILT_SOURCES = default.supp

default.supp: $(SUPP_FILES)

bzdist: dist
	gunzip -c $(PACKAGE)-$(VERSION).tar.gz | bzip2 > $(PACKAGE)-$(VERSION).tar.bz2

EXTRA_DIST = $(val_DATA) \
	PATCHES_APPLIED ACKNOWLEDGEMENTS \
	README_KDE3_FOLKS \
	README_MISSING_SYSCALL_OR_IOCTL TODO

val_PROGRAMS = valgrind.so valgrinq.so libpthread.so.0

valgrinq_so_SOURCES = vg_valgrinq_dummy.c

valgrind_so_SOURCES = \
	vg_clientfuncs.c \
	vg_scheduler.c \
	vg_clientmalloc.c \
	vg_clientperms.c \
	vg_demangle.c \
	vg_dispatch.S \
	vg_errcontext.c \
	vg_execontext.c \
	vg_from_ucode.c \
	vg_helpers.S \
	vg_main.c \
	vg_malloc2.c \
	vg_memory.c \
	vg_messages.c \
	vg_mylibc.c \
	vg_procselfmaps.c \
	vg_profile.c \
	vg_signals.c \
	vg_startup.S \
	vg_symtab2.c \
	vg_syscall_mem.c \
	vg_syscall.S \
	vg_to_ucode.c \
	vg_translate.c \
	vg_transtab.c \
	vg_valgrinq_dummy.c \
	vg_vtagops.c

valgrind_so_LDADD = \
	demangle/cp-demangle.o \
	demangle/cplus-dem.o \
	demangle/dyn-string.o \
	demangle/safe-ctype.o

include_HEADERS = valgrind.h

noinst_HEADERS = \
        vg_kerneliface.h        \
        vg_include.h            \
        vg_constants.h          \
        vg_unsafe.h


MANUAL_DEPS = $(noinst_HEADERS) $(include_HEADERS)

vg_memory.o: vg_memory.c $(MANUAL_DEPS)
	$(COMPILE) -O2 @PREFERRED_STACK_BOUNDARY@ -c $<

vg_clientmalloc.o: vg_clientmalloc.c $(MANUAL_DEPS)
	$(COMPILE) -fno-omit-frame-pointer -c $<

libpthread.so.0: vg_libpthread.c $(MANUAL_DEPS)
	$(CC) -Wall -g -O -shared -fpic -o $@ $<

valgrind.so: $(valgrind_so_OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o valgrind.so \
	$(valgrind_so_OBJECTS) $(valgrind_so_LDADD)

valgrinq.so: $(valgrinq_so_OBJECTS)
	$(CC) $(CFLAGS) -shared -o valgrinq.so $(valgrinq_so_OBJECTS)
