
SUBDIRS = . tests

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

INCLUDES = -I$(top_srcdir)/include

val_PROGRAMS = vgskin_none.so

vgskin_none_so_SOURCES 	 = nl_main.c
vgskin_none_so_LDFLAGS   = -shared

##vgskin_none.so$(EXEEXT): $(vgskin_none_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_none.so \
##		$(vgskin_none_so_OBJECTS)
