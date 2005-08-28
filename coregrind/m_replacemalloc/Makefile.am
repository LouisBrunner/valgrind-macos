include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am

noinst_LIBRARIES = \
	libreplacemalloc_core.a \
	libreplacemalloc_toolpreload.a

libreplacemalloc_core_a_SOURCES = \
	replacemalloc_core.c

libreplacemalloc_toolpreload_a_SOURCES = \
	vg_replace_malloc.c
libreplacemalloc_toolpreload_a_CFLAGS = \
	$(PIC_AM_CFLAGS)


