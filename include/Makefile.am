
SUBDIRS      = $(VG_ARCH)     $(VG_OS)     $(VG_PLATFORM)     .
DIST_SUBDIRS = $(VG_ARCH_ALL) $(VG_OS_ALL) $(VG_PLATFORM_ALL) .

EXTRA_DIST = \
	tool.h.base \
	valgrind.h.in \
	vg_profile.c

incincdir = $(includedir)/valgrind

incinc_HEADERS = \
	basic_types.h \
	tool.h \
	tool_asm.h \
	valgrind.h

BUILT_SOURCES = tool.h valgrind.h
CLEANFILES = tool.h valgrind.h

tool.h: $(srcdir)/tool.h.base \
	 $(top_srcdir)/coregrind/gen_toolint.pl $(top_srcdir)/coregrind/toolfuncs.def
	rm -f $@
	cat $(srcdir)/tool.h.base > $@
	$(PERL) $(top_srcdir)/coregrind/gen_toolint.pl toolproto < $(top_srcdir)/coregrind/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(top_srcdir)/coregrind/gen_toolint.pl initproto < $(top_srcdir)/coregrind/toolfuncs.def >> $@ || rm -f $@
