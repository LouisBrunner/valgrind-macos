
SUBDIRS = . tests docs

INCLUDES = -I$(top_srcdir)/include

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer \
		@PREFERRED_STACK_BOUNDARY@ -g \
		$(INCLUDES)

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_memcheck.so

vgskin_memcheck_so_SOURCES = \
	mc_main.c \
	mc_clientreqs.c \
	mc_errcontext.c \
	mc_from_ucode.c \
	mc_translate.c \
	mc_helpers.S
vgskin_memcheck_so_LDFLAGS = -shared

noinst_HEADERS = \
	mc_include.h	\
	memcheck.h

MANUAL_DEPS = $(noinst_HEADERS)

##vgskin_memcheck.so$(EXEEXT): $(vgskin_memcheck_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_memcheck.so \
##		$(vgskin_memcheck_so_OBJECTS)
