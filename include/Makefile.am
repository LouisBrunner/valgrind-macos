EXTRA_DIST = \
	vg_profile.c \
	tool.h.base

incincdir = $(includedir)/valgrind

incinc_HEADERS = \
	tool.h \
	tool_asm.h \
	valgrind.h \
	vg_kerneliface.h \
	vg_skin.h

BUILT_SOURCES = tool.h
CLEANFILES = tool.h

tool.h: $(srcdir)/tool.h.base \
	 $(top_srcdir)/coregrind/gen_toolint.pl $(top_srcdir)/coregrind/toolfuncs.def
	rm -f $@
	cat $(srcdir)/tool.h.base > $@
	$(PERL) $(top_srcdir)/coregrind/gen_toolint.pl toolproto < $(top_srcdir)/coregrind/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(top_srcdir)/coregrind/gen_toolint.pl initproto < $(top_srcdir)/coregrind/toolfuncs.def >> $@ || rm -f $@
