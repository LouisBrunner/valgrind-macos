include $(top_srcdir)/Makefile.tool.am

SUBDIRS += hp2ps

val_PROGRAMS = massif vgpreload_massif.so

vgpreload_massif_so_SOURCES      = 
vgpreload_massif_so_DEPENDENCIES = $(LIBREPLACEMALLOC)
vgpreload_massif_so_LDFLAGS      = \
	-shared -Wl,-z,interpose,-z,initfirst \
	-Wl,--whole-archive \
	$(LIBREPLACEMALLOC) \
	-Wl,--no-whole-archive

massif_SOURCES      = ms_main.c
massif_DEPENDENCIES = $(COREGRIND_LIBS)
massif_LDADD        = $(TOOL_LINKADD)
massif_LDFLAGS 	    = $(TOOL_LINKFLAGS)
