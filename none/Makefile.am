
SUBDIRS = . docs tests

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_none.so

vgskin_none_so_SOURCES 	 = nl_main.c
vgskin_none_so_LDFLAGS   = -shared

