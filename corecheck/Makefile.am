
SUBDIRS = . tests docs

AM_CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

INCLUDES = -I$(top_srcdir)/include

val_PROGRAMS = vgskin_corecheck.so

vgskin_corecheck_so_SOURCES = cc_main.c
vgskin_corecheck_so_LDFLAGS = -shared

##vgskin_corecheck.so$(EXEEXT): $(vgskin_corecheck_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_corecheck.so \
##		$(vgskin_corecheck_so_OBJECTS)
