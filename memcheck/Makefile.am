include $(top_srcdir)/Makefile.tool.am

## Build Memcheck at a higher optimisation level
AM_CFLAGS += -O2

val_PROGRAMS = vgskin_memcheck.so vgpreload_memcheck.so

vgpreload_memcheck_so_SOURCES = \
	mac_replace_strmem.c
vgpreload_memcheck_so_DEPENDENCIES = $(top_builddir)/coregrind/lib_replace_malloc.a
vgpreload_memcheck_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst \
	-Wl,--whole-archive \
	$(top_builddir)/coregrind/lib_replace_malloc.a \
	-Wl,--no-whole-archive

vgskin_memcheck_so_SOURCES = \
	mac_leakcheck.c \
	mac_malloc_wrappers.c \
	mac_needs.c \
	mc_main.c \
	mc_clientreqs.c \
	mc_errcontext.c \
	mc_from_ucode.c \
	mc_translate.c \
	mc_helpers.S
vgskin_memcheck_so_LDFLAGS = -shared

mcincludedir = $(includedir)/valgrind

mcinclude_HEADERS = \
	memcheck.h

noinst_HEADERS =	\
	mac_shared.h	\
	mc_asm.h	\
	mc_include.h

mac_replace_strmem.o: CFLAGS += -fno-omit-frame-pointer

