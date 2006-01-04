
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
        basic_malloc.stderr.exp basic_malloc.vgtest \
        toobig-allocs.stderr.exp toobig-allocs.vgtest \
        true_html.stderr.exp true_html.vgtest \
        true_text.stderr.exp true_text.vgtest

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)

check_PROGRAMS = \
	basic_malloc

