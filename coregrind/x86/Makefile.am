include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer -g

noinst_HEADERS = \
	core_arch.h \
	core_arch_asm.h \
	ume_archdefs.h

noinst_LIBRARIES = libarch.a

EXTRA_DIST =		\
	ume_entry.S	\
	ume_go.c

BUILT_SOURCES = stage2.lds
CLEANFILES = stage2.lds

libarch_a_SOURCES = \
	state.c

# Extract ld's default linker script and hack it to our needs
stage2.lds: Makefile
	$(CC) -Wl,--verbose -nostdlib 2>&1 | sed \
		-e '1,/^=====\+$$/d' \
		-e '/^=====\+$$/d' \
		-e 's/ENTRY(_start)/ENTRY(_ume_entry)/' \
		-e 's/0x08048000/kickstart_base/g' > $@ || rm -f $@
