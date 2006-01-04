
include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.flags.am

AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -g

val_PROGRAMS = hp2ps 

EXTRA_DIST = \
    CHANGES \
    INSTALL \
    LICENSE \
    README \
    Makefile.old \
    hp2ps.1

hp2ps_SOURCES = \
	AuxFile.c	\
	Axes.c		\
	AreaBelow.c	\
	Curves.c 	\
	Deviation.c	\
	Dimensions.c	\
	Error.c 	\
	HpFile.c	\
	Key.c		\
	Main.c 		\
	Marks.c		\
	TopTwenty.c	\
	TraceElement.c	\
	PsFile.c 	\
	Reorder.c	\
	Scale.c		\
	Shade.c 	\
	Utilities.c

# Build hp2ps for the primary target only.
hp2ps_LDADD     = -lm
hp2ps_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
hp2ps_CFLAGS    = $(AM_CFLAGS_PRI)
hp2ps_CCASFLAGS = $(AM_CCASFLAGS_PRI)
hp2ps_LDFLAGS   = $(AM_CFLAGS_PRI)

noinst_HEADERS = \
       AreaBelow.h \
       AuxFile.h \
       Axes.h \
       Curves.h \
       Defines.h \
       Deviation.h \
       Dimensions.h \
       Error.h \
       HpFile.h \
       Key.h \
       Main.h \
       Marks.h \
       PsFile.h \
       Reorder.h \
       Scale.h \
       Shade.h \
       TopTwenty.h \
       TraceElement.h \
       Utilities.h

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(addprefix $(inplacedir)/,$(val_PROGRAMS))
	ln -f -s $(addprefix ../$(subdir)/,$(val_PROGRAMS)) $(inplacedir)
