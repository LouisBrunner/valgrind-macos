noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
        basic_malloc.stderr.exp basic_malloc.vgtest \
        toobig-allocs.stderr.exp toobig-allocs.vgtest \
        true_html.stderr.exp true_html.vgtest \
        true_text.stderr.exp true_text.vgtest

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 

check_PROGRAMS = \
	basic_malloc

