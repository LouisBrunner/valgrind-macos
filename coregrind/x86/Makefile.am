include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g

noinst_HEADERS = \
	core_arch.h \
	core_arch_asm.h \
	x86_private.h

noinst_LIBRARIES = libarch.a

EXTRA_DIST = \
	jmp_with_stack.c

BUILT_SOURCES = stage2.lds
CLEANFILES = stage2.lds

libarch_a_SOURCES = \
	cpuid.S \
	helpers.S \
	dispatch.S \
	jmp_with_stack.c \
	state.c

if USE_PIE
libarch_a_CFLAGS = $(AM_CFLAGS) -fpie
else
libarch_a_CFLAGS = $(AM_CFLAGS)
endif

# Extract ld's default linker script and hack it to our needs
stage2.lds: Makefile
	$(CC) -Wl,--verbose -nostdlib 2>&1 | sed \
		-e '1,/^=====\+$$/d' \
		-e '/^=====\+$$/d' \
		-e 's/0x08048000/kickstart_base/g' > $@ || rm -f $@
