
SUBDIRS = . unused

bin_SCRIPTS = \
	vg_regtest

noinst_SCRIPTS = \
	filter_addresses \
	filter_discards \
	filter_stderr_basic \
	filter_test_paths

noinst_PROGRAMS = \
	true

CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
CXXFLAGS = $(CFLAGS)

# generic C ones
true_SOURCES 	= true.c

#EXTRA_DIST = \
#	$(noinst_SCRIPTS) \
#	blocked_syscall.c \
#	clientperm.c \
#	clientstackperm.c \
#	discard.c

EXTRA_DIST = $(noinst_SCRIPTS)

