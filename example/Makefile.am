
SUBDIRS = .

AM_CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

INCLUDES = -I$(top_srcdir)/include

val_PROGRAMS = vgskin_example.so

vgskin_example_so_SOURCES = ex_main.c
vgskin_example_so_LDFLAGS = -shared

##vgskin_example.so$(EXEEXT): $(vgskin_example_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_example.so \
##		$(vgskin_example_so_OBJECTS)
