
SUBDIRS = .

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_example.so

vgskin_example_so_SOURCES = ex_main.c
vgskin_example_so_LDFLAGS = -shared
