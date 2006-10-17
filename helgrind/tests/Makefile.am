
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	allok.stderr.exp allok.vgtest		\
	deadlock.stderr.exp deadlock.vgtest	\
	inherit.stderr.exp inherit.vgtest	\
	race.stderr.exp race.vgtest		\
	race2.stderr.exp race2.vgtest		\
	readshared.stderr.exp readshared.vgtest \
	toobig-allocs.stderr.exp toobig-allocs.vgtest

check_PROGRAMS = \
	allok deadlock inherit race race2 readshared

# force -gstabs, because we don't print symaddr for DWARF yet
# Sigh, gcc-3.4.3 on ppc64 generates bogus .stabs.  So disable it
# for now on ppc64-linux.
if VGP_PPC64_LINUX
AM_CFLAGS	= $(WERROR) -Winline -Wall -Wshadow -g      $(AM_FLAG_M3264_PRI)
else

# In fact -gstabs is broken on many systems now
#AM_CFLAGS	= $(WERROR) -Winline -Wall -Wshadow -gstabs $(AM_FLAG_M3264_PRI)
AM_CFLAGS	= $(WERROR) -Winline -Wall -Wshadow -g      $(AM_FLAG_M3264_PRI)

endif

LDADD		= -lpthread
