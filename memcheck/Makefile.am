
SUBDIRS = . tests docs

INCLUDES = -I$(top_srcdir)/include

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g

CCASFLAGS = $(INCLUDES)

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_memcheck.so

vgskin_memcheck_so_SOURCES = \
	mac_leakcheck.c \
	mac_needs.c \
	mc_main.c \
	mc_clientreqs.c \
	mc_errcontext.c \
	mc_from_ucode.c \
	mc_translate.c \
	mc_helpers.S
vgskin_memcheck_so_LDFLAGS = -shared

mcincludedir = $(includedir)/valgrind

mcinclude_HEADERS = \
	memcheck.h

noinst_HEADERS =	\
	mac_shared.h	\
	mc_constants.h	\
	mc_include.h

