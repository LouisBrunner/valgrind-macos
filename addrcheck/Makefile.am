
SUBDIRS = .

INCLUDES = -I$(top_srcdir)/include

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -Wshadow -O -fomit-frame-pointer @PREFERRED_STACK_BOUNDARY@ -g

valdir = $(libdir)/valgrind

val_PROGRAMS = vgskin_addrcheck.so

vgskin_addrcheck_so_SOURCES = ac_main.c
vgskin_addrcheck_so_LDFLAGS = -shared

noinst_HEADERS = ac_include.h

##vgskin_addrcheck.so$(EXEEXT): $(vgskin_addrcheck_so_OBJECTS)
##	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o vgskin_addrcheck.so \
##		$(vgskin_addrcheck_so_OBJECTS)

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(valdir)
	rm -f $(DESTDIR)$(valdir)/libpthread.so.0
	$(LN_S) libpthread.so $(DESTDIR)$(valdir)/libpthread.so.0

