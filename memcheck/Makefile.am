include $(top_srcdir)/Makefile.tool.am

## Build Memcheck at a higher optimisation level
AM_CFLAGS += -O2

val_PROGRAMS = memcheck vgpreload_memcheck.so

vgpreload_memcheck_so_SOURCES      = mac_replace_strmem.c
vgpreload_memcheck_so_DEPENDENCIES = $(LIBREPLACEMALLOC)
vgpreload_memcheck_so_LDFLAGS      = \
	-shared -Wl,-z,interpose,-z,initfirst \
	-Wl,--whole-archive \
	$(LIBREPLACEMALLOC) \
	-Wl,--no-whole-archive

memcheck_SOURCES = \
	mac_leakcheck.c \
	mac_malloc_wrappers.c \
	mc_main.c \
	mac_shared.c \
	mc_translate.c
memcheck_DEPENDENCIES = $(COREGRIND_LIBS)
memcheck_LDADD        = $(TOOL_LINKADD)
memcheck_LDFLAGS      = $(TOOL_LINKFLAGS)

mcincludedir = $(includedir)/valgrind

mcinclude_HEADERS = \
	memcheck.h

noinst_HEADERS =	\
	mac_shared.h	\
	mc_include.h

mac_replace_strmem.o: CFLAGS += -fno-omit-frame-pointer

mc_main.o: CFLAGS += -fomit-frame-pointer
