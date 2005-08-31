include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

noinst_LIBRARIES = libdispatch.a

# Remember to include all the arch-specific files in the distribution.
EXTRA_DIST = \
	$(addsuffix .S,$(addprefix dispatch-,$(VG_ARCH_ALL)))

libdispatch_a_SOURCES = \
	dispatch-@VG_ARCH@.S

dispatch-@VG_ARCH@.S: libvex_guest_offsets.h

libvex_guest_offsets.h:
	$(MAKE) -C @VEX_DIR@ CC="$(CC)" pub/libvex_guest_offsets.h
