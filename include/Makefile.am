EXTRA_DIST = \
	vg_profile.c \
	vg_skin.h.base

incincdir = $(includedir)/valgrind

incinc_HEADERS = \
	valgrind.h \
	vg_constants_skin.h \
	vg_skin.h \
	vg_kerneliface.h

BUILT_SOURCES = vg_skin.h
CLEANFILES = vg_skin.h

vg_skin.h: $(srcdir)/vg_skin.h.base \
	 $(top_srcdir)/coregrind/gen_toolint.pl $(top_srcdir)/coregrind/toolfuncs.def
	rm -f $@
	cat $(srcdir)/vg_skin.h.base > $@
	$(PERL) $(top_srcdir)/coregrind/gen_toolint.pl toolproto < $(top_srcdir)/coregrind/toolfuncs.def >> $@ || rm -f $@
	$(PERL) $(top_srcdir)/coregrind/gen_toolint.pl initproto < $(top_srcdir)/coregrind/toolfuncs.def >> $@ || rm -f $@
