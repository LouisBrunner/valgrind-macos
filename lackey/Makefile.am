
SUBDIRS = . docs tests

AM_CPPFLAGS = -I$(top_srcdir)/include -DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind
inplacedir = $(top_srcdir)/.in_place

val_PROGRAMS = vgskin_lackey.so

vgskin_lackey_so_SOURCES = lk_main.c
vgskin_lackey_so_LDFLAGS = -shared


all-local:
	mkdir -p $(inplacedir)
	-rm -f $(inplacedir)/$(val_PROGRAMS)
	ln -f -s $(top_srcdir)/$(subdir)/$(val_PROGRAMS) $(inplacedir)/$(val_PROGRAMS)
