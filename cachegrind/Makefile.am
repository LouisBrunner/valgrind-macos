##include $(top_srcdir)/Makefile.tool.am
include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.tool-flags.am
include $(top_srcdir)/Makefile.tool-inplace.am

SUBDIRS      = $(VG_ARCH)     . tests docs
DIST_SUBDIRS = $(VG_ARCH_ALL) . tests docs

AM_CPPFLAGS += -I$(top_srcdir)/cachegrind/$(VG_ARCH)

bin_SCRIPTS = cg_annotate

EXTRA_DIST = cg_sim.c 

noinst_HEADERS = cg_arch.h

val_PROGRAMS = vgtool_cachegrind.so

vgtool_cachegrind_so_SOURCES = cg_main.c
vgtool_cachegrind_so_LDFLAGS = -shared
vgtool_cachegrind_so_LDADD   = ${VG_ARCH}/libcgarch.a

