
SUBDIRS = .

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind
inplacedir = $(top_builddir)/.in_place

val_PROGRAMS = vgskin_example.so

vgskin_example_so_SOURCES = ex_main.c
vgskin_example_so_LDFLAGS = -shared

all-local:
	mkdir -p $(inplacedir)
	-rm -f $(addprefix $(inplacedir)/,$(val_PROGRAMS))
	ln -f -s $(addprefix $(top_builddir)/$(subdir)/,$(val_PROGRAMS)) $(inplacedir)

