include $(top_srcdir)/Makefile.tool.am

SUBDIRS += hp2ps

val_PROGRAMS = vgskin_massif.so vgpreload_massif.so

vgskin_massif_so_SOURCES = ms_main.c
vgskin_massif_so_LDFLAGS = -shared

vgpreload_massif_so_SOURCES = 
vgpreload_massif_so_LDADD = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_massif_so_DEPENDENCIES = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_massif_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

