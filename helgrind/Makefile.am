
SUBDIRS = . docs tests

AM_CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

INCLUDES = -I$(top_srcdir)/include

val_PROGRAMS = vgskin_helgrind.so

vgskin_helgrind_so_SOURCES = hg_main.c
vgskin_helgrind_so_LDFLAGS = -shared
vgskin_helgrind_so_LDADD = ../coregrind/vg_replace_malloc.o

hgincludedir = $(includedir)/valgrind

hginclude_HEADERS = helgrind.h
