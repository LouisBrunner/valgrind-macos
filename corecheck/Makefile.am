
SUBDIRS = . tests docs

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_corecheck.so

vgskin_corecheck_so_SOURCES = cc_main.c
vgskin_corecheck_so_LDFLAGS = -shared
