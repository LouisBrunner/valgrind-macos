
SUBDIRS = . docs tests

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

bin_SCRIPTS = cg_annotate

EXTRA_DIST = \
	cg_sim_I1.c \
	cg_sim_D1.c \
	cg_sim_L2.c \
	cg_sim_gen.c 

val_PROGRAMS = vgskin_cachegrind.so

vgskin_cachegrind_so_SOURCES = cg_main.c
vgskin_cachegrind_so_LDFLAGS = -shared
