
SUBDIRS = demangle .

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

INCLUDES = -I$(srcdir)/demangle -I$(top_srcdir)/include

bin_SCRIPTS = valgrind

default.supp: $(SUPP_FILES)

val_PROGRAMS = \
	valgrind.so \
	valgrinq.so \
	libpthread.so

EXTRA_DIST = vg_libpthread.vs

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

noinst_HEADERS = \
        vg_kerneliface.h        \
        vg_include.h            \
        vg_constants.h          \
        vg_unsafe.h

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
