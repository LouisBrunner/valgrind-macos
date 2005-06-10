include $(top_srcdir)/Makefile.tool.am

bin_SCRIPTS = cg_annotate

EXTRA_DIST = cg_sim.c 

noinst_HEADERS = cg_arch.h

val_PROGRAMS = vgtool_cachegrind.so

vgtool_cachegrind_so_SOURCES = \
	cg_main.c \
	cg-@VG_ARCH@.c
vgtool_cachegrind_so_LDFLAGS = -shared

