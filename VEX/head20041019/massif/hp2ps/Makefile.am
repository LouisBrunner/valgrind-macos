include $(top_srcdir)/Makefile.all.am

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
hp2ps_LDADD = -lm

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

include $(top_srcdir)/Makefile.tool-inplace.am

