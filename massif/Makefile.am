include $(top_srcdir)/Makefile.tool.am

SUBDIRS += hp2ps

val_PROGRAMS = vgtool_massif.so vgpreload_massif.so

vgtool_massif_so_SOURCES = ms_main.c
vgtool_massif_so_LDFLAGS = -shared

vgpreload_massif_so_SOURCES = 
vgpreload_massif_so_DEPENDENCIES = $(top_builddir)/coregrind/lib_replace_malloc.a
vgpreload_massif_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst \
	-Wl,--whole-archive \
	$(top_builddir)/coregrind/lib_replace_malloc.a \
	-Wl,--no-whole-archive


