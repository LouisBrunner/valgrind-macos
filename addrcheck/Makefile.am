include $(top_srcdir)/Makefile.tool.am

# include memcheck/ for mac_shared.h
AM_CPPFLAGS += -I$(top_srcdir)/memcheck

val_PROGRAMS = vgtool_addrcheck.so vgpreload_addrcheck.so

vgtool_addrcheck_so_SOURCES = ac_main.c
vgtool_addrcheck_so_LDFLAGS = -shared
vgtool_addrcheck_so_LDADD = \
	../memcheck/mac_leakcheck.o \
	../memcheck/mac_malloc_wrappers.o \
	../memcheck/mac_shared.o

vgpreload_addrcheck_so_SOURCES = 
vgpreload_addrcheck_so_LDADD = \
	../memcheck/mac_replace_strmem.o
vgpreload_addrcheck_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC) \
	../memcheck/mac_replace_strmem.o
vgpreload_addrcheck_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst \
	-Wl,--whole-archive \
	$(LIBREPLACEMALLOC) \
	-Wl,--no-whole-archive

