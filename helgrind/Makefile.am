
SUBDIRS = . docs tests

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind
inplacedir = $(top_srcdir)/.in_place

val_PROGRAMS = vgskin_helgrind.so vgpreload_helgrind.so

vgskin_helgrind_so_SOURCES = hg_main.c
vgskin_helgrind_so_LDFLAGS = -shared

vgpreload_helgrind_so_SOURCES = 
vgpreload_helgrind_so_LDADD = $(top_srcdir)/coregrind/vg_replace_malloc.o
vgpreload_helgrind_so_DEPENDENCIES = $(top_srcdir)/coregrind/vg_replace_malloc.o
vgpreload_helgrind_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

hgincludedir = $(includedir)/valgrind

hginclude_HEADERS = helgrind.h

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(addprefix $(inplacedir)/,$(val_PROGRAMS))
	ln -f -s $(addprefix $(top_srcdir)/$(subdir)/,$(val_PROGRAMS)) $(inplacedir)
