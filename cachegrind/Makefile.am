
SUBDIRS = . docs tests

AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind
inplacedir = $(top_builddir)/.in_place

bin_SCRIPTS = cg_annotate

EXTRA_DIST = cg_sim.c 

val_PROGRAMS = vgskin_cachegrind.so

vgskin_cachegrind_so_SOURCES = cg_main.c
vgskin_cachegrind_so_LDFLAGS = -shared

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(inplacedir)/$(val_PROGRAMS)
	ln -f -s $(top_builddir)/$(subdir)/$(val_PROGRAMS) $(inplacedir)/$(val_PROGRAMS)
