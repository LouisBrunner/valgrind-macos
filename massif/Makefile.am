
SUBDIRS = . tests docs hp2ps

AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include \
		-DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind
inplacedir = $(top_builddir)/.in_place

val_PROGRAMS = vgskin_massif.so vgpreload_massif.so

vgskin_massif_so_SOURCES = ms_main.c
vgskin_massif_so_LDFLAGS = -shared

vgpreload_massif_so_SOURCES = 
vgpreload_massif_so_LDADD = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_massif_so_DEPENDENCIES = $(top_builddir)/coregrind/vg_replace_malloc.o
vgpreload_massif_so_LDFLAGS = -shared -Wl,-z,interpose,-z,initfirst

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(addprefix $(inplacedir)/,$(val_PROGRAMS))
	ln -f -s $(addprefix $(top_builddir)/$(subdir)/,$(val_PROGRAMS)) $(inplacedir)
