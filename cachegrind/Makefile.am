include $(top_srcdir)/Makefile.tool.am

bin_SCRIPTS = cg_annotate

# Remember to include all the arch-specific files in the distribution.
EXTRA_DIST = \
	cg_sim.c \
	$(addsuffix .c,$(addprefix cg-,$(VG_ARCH_ALL)))

noinst_HEADERS = cg_arch.h

val_PROGRAMS = cachegrind

cachegrind_SOURCES      = cg_main.c cg-@VG_ARCH@.c
cachegrind_DEPENDENCIES = $(COREGRIND_LIBS)
cachegrind_LDADD        = $(TOOL_LINKADD)
cachegrind_LDFLAGS      = $(TOOL_LINKFLAGS)
