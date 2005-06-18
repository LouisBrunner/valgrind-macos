include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

noinst_LIBRARIES = libarch.a

BUILT_SOURCES = stage2.lds
CLEANFILES = stage2.lds

libarch_a_SOURCES = \
	cpuid.S

# Extract ld's default linker script and hack it to our needs
stage2.lds: Makefile
	$(CC) -Wl,--verbose -nostdlib 2>&1 | sed \
		-e '1,/^=====\+$$/d' \
		-e '/^=====\+$$/d' \
		-e 's/0x400000/kickstart_base/g' > $@ || rm -f $@
