include $(top_srcdir)/Makefile.tool.am

# include memcheck/ for mac_shared.h
AM_CPPFLAGS += -I$(top_srcdir)/memcheck

val_PROGRAMS = vgskin_addrcheck.so vgpreload_addrcheck.so

vgskin_addrcheck_so_SOURCES = ac_main.c
vgskin_addrcheck_so_LDFLAGS = -shared
vgskin_addrcheck_so_LDADD = \
	../memcheck/mac_leakcheck.o \
	../memcheck/mac_malloc_wrappers.o \
	../memcheck/mac_needs.o

vgpreload_addrcheck_so_SOURCES = 
vgpreload_addrcheck_so_LDADD = \
	$(top_builddir)/coregrind/vg_replace_malloc.o \
	../memcheck/mac_replace_strmem.o
vgpreload_addrcheck_so_DEPENDENCIES = \
	$(top_builddir)/coregrind/vg_replace_malloc.o \
	../memcheck/mac_replace_strmem.o
vgpreload_addrcheck_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

