noinst_HEADERS = \
	ume_archdefs.h

EXTRA_DIST =		\
	Make.inc	\
	ume_archdefs.c	\
	ume_archdefs.h	\
	ume_entry.S	\
	ume_go.c

BUILT_SOURCES = stage2.lds
CLEANFILES = stage2.lds

# Extract ld's default linker script and hack it to our needs
stage2.lds: Makefile
	$(CC) -Wl,--verbose -nostdlib 2>&1 | sed \
		-e '1,/^=====\+$$/d' \
		-e '/^=====\+$$/d' \
		-e 's/ENTRY(_start)/ENTRY(_ume_entry)/' \
		-e 's/0x08048000/kickstart_base/' > $@ || rm -f $@
