##include $(top_srcdir)/Makefile.tool.am
include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.tool-flags.am
include $(top_srcdir)/Makefile.tool-inplace.am

SUBDIRS = $(VG_ARCH) . tests docs

bin_SCRIPTS = cg_annotate

EXTRA_DIST = cg_sim.c 

noinst_HEADERS = cg_arch.h

val_PROGRAMS = vgskin_cachegrind.so

vgskin_cachegrind_so_SOURCES = cg_main.c
vgskin_cachegrind_so_LDFLAGS = -shared
vgskin_cachegrind_so_LDADD   = ${VG_ARCH}/libcgarch.a

