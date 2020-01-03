include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
   arithmetic.vgtest arithmetic.stderr.exp arithmetic.stdout.exp \
   bits.vgtest bits.stderr.exp bits.stdout.exp \
   branches.vgtest branches.stderr.exp branches.stdout.exp \
   load_store.vgtest load_store.stderr.exp load_store.stdout.exp \
   move.vgtest move.stderr.exp move.stdout.exp \
   pc_instructions.vgtest pc_instructions.stderr.exp pc_instructions.stdout.exp

check_PROGRAMS = \
   arithmetic \
   bits \
   branches \
   load_store \
   move \
   pc_instructions \
   allexec

arithmetic_CCASFLAGS      = -Wa,-mpic $(AM_CCASFLAGS)
bits_CCASFLAGS            = -Wa,-mpic $(AM_CCASFLAGS)
load_store_CCASFLAGS      = -Wa,-mpic $(AM_CCASFLAGS)
move_CCASFLAGS            = -Wa,-mpic $(AM_CCASFLAGS)
pc_instructions_CCASFLAGS = -Wa,-mpic $(AM_CCASFLAGS)

arithmetic_SOURCES      = mwrap_printf.c arithmetic.S
bits_SOURCES            = mwrap_printf.c bits.S
branches_SOURCES        = branches.c
load_store_SOURCES      = mwrap_printf.c load_store.S
move_SOURCES            = mwrap_printf.c move.S
pc_instructions_SOURCES = mwrap_printf.c pc_instructions.S
