
SUBDIRS = . docs tests

AM_CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

INCLUDES = -I$(top_srcdir)/include

val_PROGRAMS = vgskin_lackey.so

vgskin_lackey_so_SOURCES = lk_main.c
vgskin_lackey_so_LDFLAGS = -shared

##vgskin_lackey.so$(EXEEXT): $(vgskin_lackey_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_lackey.so \
##		$(vgskin_lackey_so_OBJECTS)
