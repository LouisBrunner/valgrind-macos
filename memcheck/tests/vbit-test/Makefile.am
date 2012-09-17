include $(top_srcdir)/Makefile.all.am

EXTRA_DIST = vbit-test.vgtest vbit-test.stderr.exp

dist_noinst_SCRIPTS = filter_stderr

#----------------------------------------------------------------------------
# Headers
#----------------------------------------------------------------------------

pkginclude_HEADERS = 
noinst_HEADERS = vtest.h vbits.h

#----------------------------------------------------------------------------
# vbit_test
#----------------------------------------------------------------------------

noinst_PROGRAMS = vbit-test

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

SOURCES = \
	main.c \
	unary.c \
	binary.c \
	ternary.c \
	qernary.c \
	util.c \
	vbits.c \
	irops.c \
	valgrind.c

vbit_test_SOURCES      = $(SOURCES)
vbit_test_CPPFLAGS     = $(AM_CPPFLAGS) \
                         -I$(top_srcdir)/include  \
                         -I$(top_srcdir)/memcheck \
                         -I$(top_srcdir)/VEX/pub
vbit_test_CFLAGS       = $(AM_CFLAGS) -std=c99
vbit_test_DEPENDENCIES = 
vbit_test_LDADD        = 
vbit_test_LDFLAGS      = 
vbit_test_LINK = $(LINK) 
	$(LINK) $(vbit_test_CFLAGS) $(vbit_test_LDFLAGS)
