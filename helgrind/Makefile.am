include $(top_srcdir)/Makefile.tool.am

val_PROGRAMS = vgtool_helgrind.so vgpreload_helgrind.so

vgtool_helgrind_so_SOURCES = hg_main.c
vgtool_helgrind_so_LDFLAGS = -shared

vgpreload_helgrind_so_SOURCES = 
vgpreload_helgrind_so_DEPENDENCIES = $(top_builddir)/coregrind/lib_replace_malloc.a
vgpreload_helgrind_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst \
	-Wl,--whole-archive \
	$(top_builddir)/coregrind/lib_replace_malloc.a \
	-Wl,--no-whole-archive

hgincludedir = $(includedir)/valgrind

hginclude_HEADERS = helgrind.h

