include $(top_srcdir)/Makefile.tool.am

bin_SCRIPTS = cg_annotate

# Remember to include all the arch-specific files in the distribution.
EXTRA_DIST = \
	cg_sim.c \
	$(addsuffix .c,$(addprefix cg-,$(VG_ARCH_ALL)))

noinst_HEADERS = cg_arch.h

val_PROGRAMS = vgtool_cachegrind.so

vgtool_cachegrind_so_SOURCES = \
	cg_main.c \
	cg-@VG_ARCH@.c
vgtool_cachegrind_so_LDFLAGS = -shared

