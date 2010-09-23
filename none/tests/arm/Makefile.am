
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	neon128.stdout.exp neon128.stderr.exp neon128.vgtest \
	neon64.stdout.exp neon64.stderr.exp neon64.vgtest \
	v6intARM.stdout.exp v6intARM.stderr.exp v6intARM.vgtest \
	v6intThumb.stdout.exp v6intThumb.stderr.exp v6intThumb.vgtest \
	v6media.stdout.exp v6media.stderr.exp v6media.vgtest \
	vfp.stdout.exp vfp.stderr.exp vfp.vgtest

check_PROGRAMS = \
	neon128 \
	neon64 \
	v6intARM \
	v6intThumb \
	v6media \
	vfp

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

# These two are specific to their ARM/Thumb respectively and so we
# hardwire -marm/-mthumb.  neon64 and neon128 are compilable on both,
# however, ask for them to be compiled on thumb, as that looks
# like that's going to be the more common use case.  They also
# need special helping w.r.t -mfpu and -mfloat-abi, though.
# Also force -O0 since -O takes hundreds of MB of memory 
# for v6intThumb.c.
v6intARM_CFLAGS   = $(AM_CFLAGS) -g -O0 -mcpu=cortex-a8 -marm
v6intThumb_CFLAGS = $(AM_CFLAGS) -g -O0 -mcpu=cortex-a8 -mthumb

v6media_CFLAGS    = $(AM_CFLAGS) -g -O0 -mcpu=cortex-a8 -mthumb

vfp_CFLAGS        = $(AM_CFLAGS) -g -O0 -mcpu=cortex-a8 \
			-mfpu=neon -mfloat-abi=softfp \
			-mthumb


neon128_CFLAGS    = $(AM_CFLAGS) -g -O0 -mcpu=cortex-a8 \
			-mfpu=neon -mfloat-abi=softfp \
			-mthumb

neon64_CFLAGS     = $(AM_CFLAGS) -g -O0 -mcpu=cortex-a8 \
			-mfpu=neon -mfloat-abi=softfp \
			-mthumb
