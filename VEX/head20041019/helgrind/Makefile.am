include $(top_srcdir)/Makefile.tool.am

val_PROGRAMS = vgskin_helgrind.so vgpreload_helgrind.so

vgskin_helgrind_so_SOURCES = hg_main.c
vgskin_helgrind_so_LDFLAGS = -shared

vgpreload_helgrind_so_SOURCES = 
vgpreload_helgrind_so_LDADD = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_helgrind_so_DEPENDENCIES = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_helgrind_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

hgincludedir = $(includedir)/valgrind

hginclude_HEADERS = helgrind.h

