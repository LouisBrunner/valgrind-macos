include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

INSN_TESTS = cdsg cli cu21 cu42 ltgjhe vstrc vfae vistr vstrs

check_PROGRAMS = $(INSN_TESTS) 

EXTRA_DIST = \
	$(addsuffix .stderr.exp,$(INSN_TESTS)) \
	$(addsuffix .stdout.exp,$(INSN_TESTS)) \
	$(addsuffix .vgtest,$(INSN_TESTS))

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

vstrc_CFLAGS  = $(AM_CFLAGS) -march=z13
vfae_CFLAGS   = $(AM_CFLAGS) -march=z13
vistr_CFLAGS  = $(AM_CFLAGS) -march=z13
vstrs_CFLAGS  = $(AM_CFLAGS) -march=z13
