
SUBDIRS = . docs tests

# include memcheck/ for mc_common.{c,h}
INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/memcheck

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_addrcheck.so

vgskin_addrcheck_so_SOURCES = ac_main.c
vgskin_addrcheck_so_LDFLAGS = -shared

