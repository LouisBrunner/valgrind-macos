
##------------------------------------------------------------##
# 
# The multiple-architecture stuff in this file is pretty
# cryptic.  Read docs/internals/multiple-architectures.txt
# for at least a partial explanation of what is going on.
#
##------------------------------------------------------------##

# Process this file with autoconf to produce a configure script.
#
# Define major, minor, micro and suffix here once, then reuse them
# for version number in valgrind.h and vg-entities (documentation).
# suffix must be empty for a release, otherwise it is GIT or RC1, etc.
# Also set the (expected/last) release date here.
# Do not forget to rerun ./autogen.sh
m4_define([v_major_ver], [3])
m4_define([v_minor_ver], [23])
m4_define([v_micro_ver], [0])
m4_define([v_suffix_ver], [GIT])
m4_define([v_rel_date], ["?? Apr 2024"])
m4_define([v_version],
	  m4_if(v_suffix_ver, [],
		[v_major_ver.v_minor_ver.v_micro_ver],
		[v_major_ver.v_minor_ver.v_micro_ver.v_suffix_ver]))
AC_INIT([Valgrind],[v_version],[valgrind-users@lists.sourceforge.net])

# For valgrind.h
AC_SUBST(VG_VER_MAJOR, v_major_ver)
AC_SUBST(VG_VER_MINOR, v_minor_ver)

# For docs/xml/vg-entities.xml
AC_SUBST(VG_DATE, v_rel_date)

AC_CONFIG_SRCDIR(coregrind/m_main.c)
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects])

AM_MAINTAINER_MODE

#----------------------------------------------------------------------------
# Do NOT modify these flags here. Except in feature tests in which case
# the original values must be properly restored.
#----------------------------------------------------------------------------
CFLAGS="$CFLAGS"
CXXFLAGS="$CXXFLAGS"

#----------------------------------------------------------------------------
# Checks for various programs.
#----------------------------------------------------------------------------

AC_PROG_LN_S
m4_version_prereq([2.70], [AC_PROG_CC], [AC_PROG_CC_C99])
# Make sure we can compile in C99 mode.
if test "$ac_cv_prog_cc_c99" = "no"; then
    AC_MSG_ERROR([Valgrind relies on a C compiler supporting C99])
fi
AC_PROG_CPP
AC_PROG_CXX
# AC_PROG_OBJC apparently causes problems on older Linux distros (eg. with
# autoconf 2.59).  If we ever have any Objective-C code in the Valgrind code
# base (eg. most likely as Darwin-specific tests) we'll need one of the
# following:
# - put AC_PROG_OBJC in a Darwin-specific part of this file
# - Use AC_PROG_OBJC here and up the minimum autoconf version
# - Use the following, which is apparently equivalent:
#     m4_ifdef([AC_PROG_OBJC],
#        [AC_PROG_OBJC],
#        [AC_CHECK_TOOL([OBJC], [gcc])
#         AC_SUBST([OBJC])
#         AC_SUBST([OBJCFLAGS])
#        ])
AC_PROG_RANLIB
# Set LTO_RANLIB variable to an lto enabled ranlib
if test "x$LTO_RANLIB" = "x"; then
  AC_PATH_PROGS([LTO_RANLIB], [gcc-ranlib])
fi
AC_ARG_VAR([LTO_RANLIB],[Library indexer command for link time optimisation])

# provide a very basic definition for AC_PROG_SED if it's not provided by
# autoconf (as e.g. in autoconf 2.59).
m4_ifndef([AC_PROG_SED],
          [AC_DEFUN([AC_PROG_SED],
                    [AC_ARG_VAR([SED])
                     AC_CHECK_PROGS([SED],[gsed sed])])])
AC_PROG_SED

AC_DEFUN([AC_PROG_SHA256SUM],
          [AC_ARG_VAR([SHA256SUM])
          AC_CHECK_PROGS([SHA256SUM],[gsha256sum sha256sum])])
AC_PROG_SHA256SUM

# If no AR variable was specified, look up the name of the archiver. Otherwise
# do not touch the AR variable.
if test "x$AR" = "x"; then
  AC_PATH_PROGS([AR], [`echo $LD | $SED 's/ld$/ar/'` "ar"], [ar])
fi
AC_ARG_VAR([AR],[Archiver command])

# same for LTO_AR variable for lto enabled archiver
if test "x$LTO_AR" = "x"; then
  AC_PATH_PROGS([LTO_AR], [gcc-ar])
fi
AC_ARG_VAR([LTO_AR],[Archiver command for link time optimisation])

# figure out where perl lives
AC_PATH_PROG(PERL, perl)

# figure out where gdb lives
AC_PATH_PROG(GDB, gdb, "/no/gdb/was/found/at/configure/time")
AC_DEFINE_UNQUOTED(GDB_PATH, "$GDB", [path to GDB])

# some older automake's don't have it so try something on our own
ifdef([AM_PROG_AS],[AM_PROG_AS],
[
AS="${CC}"
AC_SUBST(AS)

ASFLAGS=""
AC_SUBST(ASFLAGS)
])


# Check if 'diff' supports -u (universal diffs) and use it if possible.

AC_MSG_CHECKING([for diff -u])
AC_SUBST(DIFF)

# Comparing two identical files results in 0.
tmpfile="tmp-xxx-yyy-zzz"
touch $tmpfile;
if diff -u $tmpfile $tmpfile ; then
    AC_MSG_RESULT([yes])
    DIFF="diff -u"
else
    AC_MSG_RESULT([no])
    DIFF="diff"
fi
rm $tmpfile

# We don't want gcc < 3.0
AC_MSG_CHECKING([for a supported version of gcc])

# Obtain the compiler version.
# 
# A few examples of how the ${CC} --version output looks like:
#
# ######## gcc variants ########
# Arch Linux: i686-pc-linux-gnu-gcc (GCC) 4.6.2
# Debian Linux: gcc (Debian 4.3.2-1.1) 4.3.2
# openSUSE: gcc (SUSE Linux) 4.5.1 20101208 [gcc-4_5-branch revision 167585]
# Exherbo Linux: x86_64-pc-linux-gnu-gcc (Exherbo gcc-4.6.2) 4.6.2
# MontaVista Linux for ARM: arm-none-linux-gnueabi-gcc (Sourcery G++ Lite 2009q1-203) 4.3.3
# OS/X 10.6: i686-apple-darwin10-gcc-4.2.1 (GCC) 4.2.1 (Apple Inc. build 5666) (dot 3)
# OS/X 10.7: i686-apple-darwin11-llvm-gcc-4.2 (GCC) 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2335.15.00)
#
# ######## clang variants ########
# Clang: clang version 2.9 (tags/RELEASE_29/final)
# Apple clang: Apple clang version 3.1 (tags/Apple/clang-318.0.58) (based on LLVM 3.1svn)
# FreeBSD clang: FreeBSD clang version 3.1 (branches/release_31 156863) 20120523
#
# ######## Apple LLVM variants ########
# Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)
# Apple LLVM version 6.0 (clang-600.0.51) (based on LLVM 3.5svn)
#
[
if test "x`${CC} --version | $SED -n -e 's/.*\Apple \(LLVM\) version.*clang.*/\1/p'`" = "xLLVM" ;
then
    is_clang="applellvm"
    gcc_version=`${CC} --version | $SED -n -e 's/.*LLVM version \([0-9.]*\).*$/\1/p'`
elif test "x`${CC} --version | $SED -n -e 's/.*\(clang\) version.*/\1/p'`" = "xclang" ;
then
    is_clang="clang"
    # Don't use -dumpversion with clang: it will always produce "4.2.1".
    gcc_version=`${CC} --version | $SED -n -e 's/.*clang version \([0-9.]*\).*$/\1/p'`
elif test "x`${CC} --version | $SED -n -e 's/icc.*\(ICC\).*/\1/p'`" = "xICC" ; 
then
    is_clang="icc"
    gcc_version=`${CC} -dumpversion 2>/dev/null`
else
    is_clang="notclang"
    gcc_version=`${CC} -dumpversion 2>/dev/null`
    if test "x$gcc_version" = x; then
	gcc_version=`${CC} --version | $SED -n -e 's/[^ ]*gcc[^ ]* ([^)]*) \([0-9.]*\).*$/\1/p'`
    fi
fi
]
AM_CONDITIONAL(COMPILER_IS_CLANG, test $is_clang = clang -o $is_clang = applellvm)
AM_CONDITIONAL(COMPILER_IS_ICC, test $is_clang = icc)

# Note: m4 arguments are quoted with [ and ] so square brackets in shell
# statements have to be quoted.
case "${is_clang}-${gcc_version}" in
     applellvm-5.1|applellvm-[[6-9]].*|applellvm-[[1-9][0-9]]*)
	AC_MSG_RESULT([ok (Apple LLVM version ${gcc_version})])
	;;
     icc-1[[3-9]].*|icc-202[[0-9]].*)
	AC_MSG_RESULT([ok (ICC version ${gcc_version})])
	;;
     notclang-[[3-9]]|notclang-[[3-9]].*|notclang-[[1-9][0-9]]*)
	AC_MSG_RESULT([ok (${gcc_version})])
	;;
     clang-2.9|clang-[[3-9]].*|clang-[[1-9][0-9]]*)
	AC_MSG_RESULT([ok (clang-${gcc_version})])
	;;
     *)
	AC_MSG_RESULT([no (${is_clang}-${gcc_version})])
	AC_MSG_ERROR([please use gcc >= 3.0 or clang >= 2.9 or icc >= 13.0 or Apple LLVM >= 5.1])
	;;
esac

#----------------------------------------------------------------------------
# Arch/OS/platform tests.
#----------------------------------------------------------------------------
# We create a number of arch/OS/platform-related variables.  We prefix them
# all with "VGCONF_" which indicates that they are defined at
# configure-time, and distinguishes them from the VGA_*/VGO_*/VGP_*
# variables used when compiling C files.

AC_CANONICAL_HOST

AC_MSG_CHECKING([for a supported CPU])

# ARCH_MAX reflects the most that this CPU can do: for example if it
# is a 64-bit capable PowerPC, then it must be set to ppc64 and not ppc32.
# Ditto for amd64.  It is used for more configuration below, but is not used
# outside this file.
#
# Power PC returns powerpc for Big Endian.  This was not changed when Little
# Endian support was added to the 64-bit architecture.  The 64-bit Little
# Endian systems explicitly state le in the host_cpu.  For clarity in the
# Valgrind code, the ARCH_MAX name will state LE or BE for the endianness of
# the 64-bit system.  Big Endian is the only mode supported on 32-bit Power PC.
# The abreviation PPC or ppc refers to 32-bit and 64-bit systems with either
# Endianness.  The name PPC64 or ppc64 to 64-bit systems of either Endianness.
# The names ppc64be or PPC64BE refer to only 64-bit systems that are Big
# Endian.  Similarly, ppc64le or PPC64LE refer to only 64-bit systems that are
# Little Endian.

VGCONF_PLATFORM_ARM_ARCH=

case "${host_cpu}" in
     i?86) 
	AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="x86"
        ;;

     x86_64|amd64) 
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="amd64"
        ;;

     powerpc64)
     # this only referrs to 64-bit Big Endian
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="ppc64be"
        ;;

     powerpc64le)
     # this only referrs to 64-bit Little Endian
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="ppc64le"
        ;;

     powerpc)
        # On Linux this means only a 32-bit capable CPU.
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="ppc32"
        ;;

     s390x)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="s390x"
        ;;

     armv8*)
	AC_MSG_RESULT([ok (${host_cpu})])
	VGCONF_PLATFORM_ARM_ARCH="-marm -mcpu=cortex-a8"
	ARCH_MAX="arm"
	;;

     armv7*)
	AC_MSG_RESULT([ok (${host_cpu})])
	VGCONF_PLATFORM_ARM_ARCH="-marm -mcpu=cortex-a8"
	ARCH_MAX="arm"
	;;

     arm*)
	AC_MSG_RESULT([ok (${host_cpu})])
	VGCONF_PLATFORM_ARM_ARCH="-march=armv6"
	ARCH_MAX="arm"
	;;

     aarch64*)
       AC_MSG_RESULT([ok (${host_cpu})])
       ARCH_MAX="arm64"
       ;;

     mips)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="mips32"
        ;;

     mipsel)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="mips32"
        ;;

     mipsisa32r2)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="mips32"
        ;;

     mips64*)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="mips64"
        ;;

     mipsisa64*)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="mips64"
        ;;
     nanomips)
        AC_MSG_RESULT([ok (${host_cpu})])
        ARCH_MAX="nanomips"
        ;;

     *) 
	AC_MSG_RESULT([no (${host_cpu})])
	AC_MSG_ERROR([Unsupported host architecture. Sorry])
	;;
esac

AC_SUBST(VGCONF_PLATFORM_ARM_ARCH)

#----------------------------------------------------------------------------

# Sometimes it's convenient to subvert the bi-arch build system and
# just have a single build even though the underlying platform is
# capable of both.  Hence handle --enable-only64bit and
# --enable-only32bit.  Complain if both are issued :-)
# [Actually, if either of these options are used, I think both get built,
# but only one gets installed.  So if you use an in-place build, both can be
# used. --njn]

# Check if a 64-bit only build has been requested
AC_CACHE_CHECK([for a 64-bit only build], vg_cv_only64bit,
   [AC_ARG_ENABLE(only64bit, 
      [  --enable-only64bit      do a 64-bit only build],
      [vg_cv_only64bit=$enableval],
      [vg_cv_only64bit=no])])

# Check if a 32-bit only build has been requested
AC_CACHE_CHECK([for a 32-bit only build], vg_cv_only32bit,
   [AC_ARG_ENABLE(only32bit, 
      [  --enable-only32bit      do a 32-bit only build],
      [vg_cv_only32bit=$enableval],
      [vg_cv_only32bit=no])])

# Stay sane
if test x$vg_cv_only64bit = xyes -a x$vg_cv_only32bit = xyes; then
   AC_MSG_ERROR(
      [Nonsensical: both --enable-only64bit and --enable-only32bit.])
fi

#----------------------------------------------------------------------------

# VGCONF_OS is the primary build OS, eg. "linux".  It is passed in to
# compilation of many C files via -VGO_$(VGCONF_OS) and
# -VGP_$(VGCONF_ARCH_PRI)_$(VGCONF_OS).
AC_MSG_CHECKING([for a supported OS])
AC_SUBST(VGCONF_OS)

DEFAULT_SUPP=""

case "${host_os}" in
     *linux*)
	AC_MSG_RESULT([ok (${host_os})])
        VGCONF_OS="linux"

        # Ok, this is linux. Check the kernel version
        AC_MSG_CHECKING([for the kernel version])

        kernel=`uname -r`

        case "${kernel}" in
             0.*|1.*|2.0.*|2.1.*|2.2.*|2.3.*|2.4.*|2.5.*) 
        	    AC_MSG_RESULT([unsupported (${kernel})])
        	    AC_MSG_ERROR([Valgrind needs a Linux kernel >= 2.6])
        	    ;;

             *)
        	    AC_MSG_RESULT([2.6 or later (${kernel})])
                    ;;
        esac

        ;;

     *freebsd*)
        AC_MSG_RESULT([ok (${host_os})])
        VGCONF_OS="freebsd"
        AC_DEFINE([FREEBSD_10], 1000, [FREEBSD_VERS value for FreeBSD 10.x])
        freebsd_10=1000
        AC_DEFINE([FREEBSD_11], 1100, [FREEBSD_VERS value for FreeBSD 11.x])
        freebsd_11=1100
        AC_DEFINE([FREEBSD_12], 1200, [FREEBSD_VERS value for FreeBSD 12.0 to 12.1])
        freebsd_12=1200
        AC_DEFINE([FREEBSD_12_2], 1220, [FREEBSD_VERS value for FreeBSD 12.2])
        freebsd_12_2=1220
        AC_DEFINE([FREEBSD_13_0], 1300, [FREEBSD_VERS value for FreeBSD 13.0])
        freebsd_13_0=1300
        AC_DEFINE([FREEBSD_13_1], 1310, [FREEBSD_VERS value for FreeBSD 13.1])
        freebsd_13_1=1310
        AC_DEFINE([FREEBSD_13_2], 1320, [FREEBSD_VERS value for FreeBSD 13.2])
        freebsd_13_2=1320
        AC_DEFINE([FREEBSD_13_3], 1330, [FREEBSD_VERS value for FreeBSD 13.3])
        freebsd_13_3=1330
        AC_DEFINE([FREEBSD_14], 1400, [FREEBSD_VERS value for FreeBSD 14.x])
        freebsd_14=1400
        AC_DEFINE([FREEBSD_15], 1500, [FREEBSD_VERS value for FreeBSD 15.x])
        freebsd_15=1500

        AC_MSG_CHECKING([for the kernel version])
        kernel=`uname -r`

        case "${kernel}" in
        10.*)
           AC_MSG_RESULT([FreeBSD 10.x (${kernel})])
           AC_DEFINE([FREEBSD_VERS], FREEBSD_10, [FreeBSD version])
           freebsd_vers=$freebsd_10
           ;;
        11.*)
           AC_MSG_RESULT([FreeBSD 11.x (${kernel})])
           AC_DEFINE([FREEBSD_VERS], FREEBSD_11, [FreeBSD version])
           freebsd_vers=$freebsd_11
           ;;
        12.*)
           case "${kernel}" in
           12.[[0-1]]-*)
              AC_MSG_RESULT([FreeBSD 12.x (${kernel})])
              AC_DEFINE([FREEBSD_VERS], FREEBSD_12, [FreeBSD version])
              freebsd_vers=$freebsd_12
              ;;
           *)
              AC_MSG_RESULT([FreeBSD 12.x (${kernel})])
              AC_DEFINE([FREEBSD_VERS], FREEBSD_12_2, [FreeBSD version])
              freebsd_vers=$freebsd_12_2
              ;;
           esac
           ;;
        13.*)
           case "${kernel}" in
           13.0-*)
              AC_MSG_RESULT([FreeBSD 13.0 (${kernel})])
              AC_DEFINE([FREEBSD_VERS], FREEBSD_13_0, [FreeBSD version])
              freebsd_vers=$freebsd_13_0
              ;;
           13.1-*)
              AC_MSG_RESULT([FreeBSD 13.1 (${kernel})])
              AC_DEFINE([FREEBSD_VERS], FREEBSD_13_1, [FreeBSD version])
              freebsd_vers=$freebsd_13_1
              ;;
           13.2-*)
              AC_MSG_RESULT([FreeBSD 13.2 (${kernel})])
              AC_DEFINE([FREEBSD_VERS], FREEBSD_13_2, [FreeBSD version])
              freebsd_vers=$freebsd_13_2
              ;;
           13.3-*)
              AC_MSG_RESULT([FreeBSD 13.3 (${kernel})])
              AC_DEFINE([FREEBSD_VERS], FREEBSD_13_3, [FreeBSD version])
              freebsd_vers=$freebsd_13_3
              ;;
           *)
              AC_MSG_RESULT([unsupported (${kernel})])
              AC_MSG_ERROR([Valgrind works on FreeBSD 10.x to 15.x])
              ;;
           esac
           ;;
        14.*)
           AC_MSG_RESULT([FreeBSD 14.x (${kernel})])
           AC_DEFINE([FREEBSD_VERS], FREEBSD_14, [FreeBSD version])
           freebsd_vers=$freebsd_14
           ;;
        15.*)
           AC_MSG_RESULT([FreeBSD 15.x (${kernel})])
           AC_DEFINE([FREEBSD_VERS], FREEBSD_15, [FreeBSD version])
           freebsd_vers=$freebsd_15
           ;;
        *)
           AC_MSG_RESULT([unsupported (${kernel})])
           AC_MSG_ERROR([Valgrind works on FreeBSD 10.x to 15.x])
           ;;
        esac

        DEFAULT_SUPP="$srcdir/freebsd.supp $srcdir/freebsd-helgrind.supp $srcdir/freebsd-drd.supp ${DEFAULT_SUPP}"
        ;;

     *darwin*)
        AC_MSG_RESULT([ok (${host_os})])
        VGCONF_OS="darwin"
        AC_DEFINE([DARWIN_10_5], 100500, [DARWIN_VERS value for Mac OS X 10.5])
        AC_DEFINE([DARWIN_10_6], 100600, [DARWIN_VERS value for Mac OS X 10.6])
        AC_DEFINE([DARWIN_10_7], 100700, [DARWIN_VERS value for Mac OS X 10.7])
        AC_DEFINE([DARWIN_10_8], 100800, [DARWIN_VERS value for Mac OS X 10.8])
        AC_DEFINE([DARWIN_10_9], 100900, [DARWIN_VERS value for Mac OS X 10.9])
        AC_DEFINE([DARWIN_10_10], 101000, [DARWIN_VERS value for Mac OS X 10.10])
        AC_DEFINE([DARWIN_10_11], 101100, [DARWIN_VERS value for Mac OS X 10.11])
        AC_DEFINE([DARWIN_10_12], 101200, [DARWIN_VERS value for macOS 10.12])
        AC_DEFINE([DARWIN_10_13], 101300, [DARWIN_VERS value for macOS 10.13])

	AC_MSG_CHECKING([for the kernel version])
	kernel=`uname -r`

        # Nb: for Darwin we set DEFAULT_SUPP here.  That's because Darwin
        # has only one relevant version, the OS version. The `uname` check
        # is a good way to get that version (i.e. "Darwin 9.6.0" is Mac OS
        # X 10.5.6, and "Darwin 10.x" is Mac OS X 10.6.x Snow Leopard,
        # and possibly "Darwin 11.x" is Mac OS X 10.7.x Lion), 
        # and we don't know of an macros similar to __GLIBC__ to get that info.
        #
        # XXX: `uname -r` won't do the right thing for cross-compiles, but
        # that's not a problem yet.
        #
        # jseward 21 Sept 2011: I seriously doubt whether V 3.7.0 will work
        # on OS X 10.5.x; I haven't tested yet, and only plan to test 3.7.0
        # on 10.6.8 and 10.7.1.  Although tempted to delete the configure
        # time support for 10.5 (the 9.* pattern just below), I'll leave it
        # in for now, just in case anybody wants to give it a try.  But I'm
        # assuming that 3.7.0 is a Snow Leopard and Lion-only release.
	case "${kernel}" in
	     9.*)
		  AC_MSG_RESULT([Darwin 9.x (${kernel}) / Mac OS X 10.5 Leopard])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_5, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin9.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin9-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     10.*)
		  AC_MSG_RESULT([Darwin 10.x (${kernel}) / Mac OS X 10.6 Snow Leopard])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_6, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin10.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     11.*)
		  AC_MSG_RESULT([Darwin 11.x (${kernel}) / Mac OS X 10.7 Lion])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_7, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin11.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     12.*)
		  AC_MSG_RESULT([Darwin 12.x (${kernel}) / Mac OS X 10.8 Mountain Lion])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_8, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin12.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     13.*)
		  AC_MSG_RESULT([Darwin 13.x (${kernel}) / Mac OS X 10.9 Mavericks])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_9, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin13.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     14.*)
		  AC_MSG_RESULT([Darwin 14.x (${kernel}) / Mac OS X 10.10 Yosemite])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_10, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin14.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     15.*)
		  AC_MSG_RESULT([Darwin 15.x (${kernel}) / Mac OS X 10.11 El Capitan])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_11, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin15.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     16.*)
		  AC_MSG_RESULT([Darwin 16.x (${kernel}) / macOS 10.12 Sierra])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_12, [Darwin / Mac OS X version])
                  DEFAULT_SUPP="$srcdir/darwin16.supp ${DEFAULT_SUPP}"
                  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
	     17.*)
		  AC_MSG_RESULT([Darwin 17.x (${kernel}) / macOS 10.13 High Sierra])
		  AC_DEFINE([DARWIN_VERS], DARWIN_10_13, [Darwin / Mac OS X version])
		  DEFAULT_SUPP="$srcdir/darwin17.supp ${DEFAULT_SUPP}"
		  DEFAULT_SUPP="$srcdir/darwin10-drd.supp ${DEFAULT_SUPP}"
		  ;;
             *) 
		  AC_MSG_RESULT([unsupported (${kernel})])
		  AC_MSG_ERROR([Valgrind works on Darwin 10.x, 11.x, 12.x, 13.x, 14.x, 15.x, 16.x and 17.x (Mac OS X 10.6/7/8/9/10/11 and macOS 10.12/13)])
		  ;;
	esac
        ;;

     solaris2.11*)
        AC_MSG_RESULT([ok (${host_os})])
        VGCONF_OS="solaris"

        uname_v=$( uname -v )
        case "$uname_v" in
             11.4.*)
                 DEFAULT_SUPP="$srcdir/solaris12.supp ${DEFAULT_SUPP}"
                 ;;
             *)
                 DEFAULT_SUPP="$srcdir/solaris11.supp ${DEFAULT_SUPP}"
                 ;;
	esac
        ;;

     solaris2.12*)
        AC_MSG_RESULT([ok (${host_os})])
        VGCONF_OS="solaris"
        DEFAULT_SUPP="$srcdir/solaris12.supp ${DEFAULT_SUPP}"
        ;;

     *) 
	AC_MSG_RESULT([no (${host_os})])
	AC_MSG_ERROR([Valgrind is operating system specific. Sorry.])
	;;
esac

#----------------------------------------------------------------------------

# If we are building on a 64 bit platform test to see if the system
# supports building 32 bit programs and disable 32 bit support if it
# does not support building 32 bit programs

case "$ARCH_MAX-$VGCONF_OS" in
     amd64-linux|ppc64be-linux|arm64-linux|amd64-solaris)
        AC_MSG_CHECKING([for 32 bit build support])
        safe_CFLAGS=$CFLAGS
        CFLAGS="-m32"
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
          return 0;
        ]])], [
        AC_MSG_RESULT([yes])
        ], [
        vg_cv_only64bit="yes"
        AC_MSG_RESULT([no])
        ])
        CFLAGS=$safe_CFLAGS;;
    mips64-linux)
        AC_MSG_CHECKING([for 32 bit build support])
        safe_CFLAGS=$CFLAGS
        CFLAGS="$CFLAGS -mips32 -mabi=32"
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <sys/prctl.h>
        ]], [[]])], [
        AC_MSG_RESULT([yes])
        ], [
        vg_cv_only64bit="yes"
        AC_MSG_RESULT([no])
        ])
        CFLAGS=$safe_CFLAGS;;
esac

if test x$vg_cv_only64bit = xyes -a x$vg_cv_only32bit = xyes; then
   AC_MSG_ERROR(
      [--enable-only32bit was specified but system does not support 32 bit builds])
fi

#----------------------------------------------------------------------------

# VGCONF_ARCH_PRI is the arch for the primary build target, eg. "amd64".  By
# default it's the same as ARCH_MAX.  But if, say, we do a build on an amd64
# machine, but --enable-only32bit has been requested, then ARCH_MAX (see
# above) will be "amd64" since that reflects the most that this cpu can do,
# but VGCONF_ARCH_PRI will be downgraded to "x86", since that reflects the
# arch corresponding to the primary build (VGCONF_PLATFORM_PRI_CAPS).  It is
# passed in to compilation of many C files via -VGA_$(VGCONF_ARCH_PRI) and
# -VGP_$(VGCONF_ARCH_PRI)_$(VGCONF_OS).
AC_SUBST(VGCONF_ARCH_PRI)

# VGCONF_ARCH_SEC is the arch for the secondary build target, eg. "x86".
# It is passed in to compilation of many C files via -VGA_$(VGCONF_ARCH_SEC)
# and -VGP_$(VGCONF_ARCH_SEC)_$(VGCONF_OS), if there is a secondary target.
# It is empty if there is no secondary target.
AC_SUBST(VGCONF_ARCH_SEC)

# VGCONF_PLATFORM_PRI_CAPS is the primary build target, eg. "AMD64_LINUX".
# The entire system, including regression and performance tests, will be
# built for this target.  The "_CAPS" indicates that the name is in capital
# letters, and it also uses '_' rather than '-' as a separator, because it's
# used to create various Makefile variables, which are all in caps by
# convention and cannot contain '-' characters.  This is in contrast to
# VGCONF_ARCH_PRI and VGCONF_OS which are not in caps.
AC_SUBST(VGCONF_PLATFORM_PRI_CAPS)

# VGCONF_PLATFORM_SEC_CAPS is the secondary build target, if there is one.
# Valgrind and tools will also be built for this target, but not the
# regression or performance tests.
#
# By default, the primary arch is the same as the "max" arch, as commented
# above (at the definition of ARCH_MAX).  We may choose to downgrade it in
# the big case statement just below here, in the case where we're building
# on a 64 bit machine but have been requested only to do a 32 bit build.
AC_SUBST(VGCONF_PLATFORM_SEC_CAPS)

AC_MSG_CHECKING([for a supported CPU/OS combination])

# NB.  The load address for a given platform may be specified in more 
# than one place, in some cases, depending on whether we're doing a biarch,
# 32-bit only or 64-bit only build.  eg see case for amd64-linux below.
# Be careful to give consistent values in all subcases.  Also, all four
# valt_load_addres_{pri,sec}_{norml,inner} values must always be set,
# even if it is to "0xUNSET".
#
case "$ARCH_MAX-$VGCONF_OS" in
     x86-linux)
        VGCONF_ARCH_PRI="x86"
        VGCONF_ARCH_SEC=""
	VGCONF_PLATFORM_PRI_CAPS="X86_LINUX"
	VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x58000000"
        valt_load_address_pri_inner="0x38000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
	;;
     amd64-linux)
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
	if test x$vg_cv_only64bit = xyes; then
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC=""
	   VGCONF_PLATFORM_PRI_CAPS="AMD64_LINUX"
	   VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
	elif test x$vg_cv_only32bit = xyes; then
	   VGCONF_ARCH_PRI="x86"
           VGCONF_ARCH_SEC=""
	   VGCONF_PLATFORM_PRI_CAPS="X86_LINUX"
	   VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
	else
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC="x86"
	   VGCONF_PLATFORM_PRI_CAPS="AMD64_LINUX"
	   VGCONF_PLATFORM_SEC_CAPS="X86_LINUX"
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
           valt_load_address_sec_norml="0x58000000"
           valt_load_address_sec_inner="0x38000000"
	fi
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
	;;
     ppc32-linux)
        VGCONF_ARCH_PRI="ppc32"
        VGCONF_ARCH_SEC=""
	VGCONF_PLATFORM_PRI_CAPS="PPC32_LINUX"
	VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x58000000"
        valt_load_address_pri_inner="0x38000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
	;;
     ppc64be-linux)
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
	if test x$vg_cv_only64bit = xyes; then
	   VGCONF_ARCH_PRI="ppc64be"
           VGCONF_ARCH_SEC=""
	   VGCONF_PLATFORM_PRI_CAPS="PPC64BE_LINUX"
	   VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
	elif test x$vg_cv_only32bit = xyes; then
	   VGCONF_ARCH_PRI="ppc32"
           VGCONF_ARCH_SEC=""
	   VGCONF_PLATFORM_PRI_CAPS="PPC32_LINUX"
	   VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
	else
	   VGCONF_ARCH_PRI="ppc64be"
           VGCONF_ARCH_SEC="ppc32"
	   VGCONF_PLATFORM_PRI_CAPS="PPC64BE_LINUX"
	   VGCONF_PLATFORM_SEC_CAPS="PPC32_LINUX"
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
           valt_load_address_sec_norml="0x58000000"
           valt_load_address_sec_inner="0x38000000"
	fi
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
	;;
     ppc64le-linux)
        # Little Endian is only supported on PPC64
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        VGCONF_ARCH_PRI="ppc64le"
        VGCONF_ARCH_SEC=""
        VGCONF_PLATFORM_PRI_CAPS="PPC64LE_LINUX"
        VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x58000000"
        valt_load_address_pri_inner="0x38000000"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
       ;;
     x86-freebsd)
        VGCONF_ARCH_PRI="x86"
        VGCONF_ARCH_SEC=""
        VGCONF_PLATFORM_PRI_CAPS="X86_FREEBSD"
        VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x38000000"
        valt_load_address_pri_inner="0x28000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     amd64-freebsd)
        if test x$vg_cv_only64bit = xyes; then
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC=""
           VGCONF_PLATFORM_PRI_CAPS="AMD64_FREEBSD"
           VGCONF_PLATFORM_SEC_CAPS=""
        elif test x$vg_cv_only32bit = xyes; then
           VGCONF_ARCH_PRI="x86"
           VGCONF_ARCH_SEC=""
           VGCONF_PLATFORM_PRI_CAPS="X86_FREEBSD"
           VGCONF_PLATFORM_SEC_CAPS=""
        else
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC="x86"
           VGCONF_PLATFORM_PRI_CAPS="AMD64_FREEBSD"
           VGCONF_PLATFORM_SEC_CAPS="X86_FREEBSD"
        fi
	# These work with either base clang or ports installed gcc
	# Hand rolled compilers probably need INSTALL_DIR/lib (at least for gcc)
        if test x$is_clang = xclang ; then
           FLAG_32ON64="-B/usr/lib32"
        else
           GCC_MAJOR_VERSION=`${CC} -dumpversion | $SED 's/\..*//' 2>/dev/null`
           FLAG_32ON64="-B/usr/local/lib32/gcc${GCC_MAJOR_VERSION} -Wl,-rpath,/usr/local/lib32/gcc${GCC_MAJOR_VERSION}/"
           FLAG_32ON64_GXX="-L/usr/local/lib32/gcc${GCC_MAJOR_VERSION} -lgcc_s"
           AC_SUBST(FLAG_32ON64_GXX)
        fi
        valt_load_address_pri_norml="0x38000000"
        valt_load_address_pri_inner="0x28000000"
        valt_load_address_sec_norml="0x38000000"
        valt_load_address_sec_inner="0x28000000"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     # Darwin gets identified as 32-bit even when it supports 64-bit.
     # (Not sure why, possibly because 'uname' returns "i386"?)  Just about
     # all Macs support both 32-bit and 64-bit, so we just build both.  If
     # someone has a really old 32-bit only machine they can (hopefully?)
     # build with --enable-only32bit.  See bug 243362.
     x86-darwin|amd64-darwin)
        ARCH_MAX="amd64"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
	if test x$vg_cv_only64bit = xyes; then
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC=""
	   VGCONF_PLATFORM_PRI_CAPS="AMD64_DARWIN"
	   VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x158000000"
           valt_load_address_pri_inner="0x138000000"
	elif test x$vg_cv_only32bit = xyes; then
           VGCONF_ARCH_PRI="x86"
           VGCONF_ARCH_SEC=""
	   VGCONF_PLATFORM_PRI_CAPS="X86_DARWIN"
	   VGCONF_PLATFORM_SEC_CAPS=""
	   VGCONF_ARCH_PRI_CAPS="x86"
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
	else
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC="x86"
	   VGCONF_PLATFORM_PRI_CAPS="AMD64_DARWIN"
	   VGCONF_PLATFORM_SEC_CAPS="X86_DARWIN"
           valt_load_address_pri_norml="0x158000000"
           valt_load_address_pri_inner="0x138000000"
           valt_load_address_sec_norml="0x58000000"
           valt_load_address_sec_inner="0x38000000"
	fi
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
	;;
     arm-linux) 
	VGCONF_ARCH_PRI="arm"
	VGCONF_PLATFORM_PRI_CAPS="ARM_LINUX"
	VGCONF_PLATFORM_SEC_CAPS=""
	valt_load_address_pri_norml="0x58000000"
	valt_load_address_pri_inner="0x38000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
	AC_MSG_RESULT([ok (${host_cpu}-${host_os})])
	;;
     arm64-linux)
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        if test x$vg_cv_only64bit = xyes; then
           VGCONF_ARCH_PRI="arm64"
           VGCONF_ARCH_SEC=""
           VGCONF_PLATFORM_PRI_CAPS="ARM64_LINUX"
           VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
        elif test x$vg_cv_only32bit = xyes; then
           VGCONF_ARCH_PRI="arm"
           VGCONF_ARCH_SEC=""
           VGCONF_PLATFORM_PRI_CAPS="ARM_LINUX"
           VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
        else
           VGCONF_ARCH_PRI="arm64"
           VGCONF_ARCH_SEC="arm"
           VGCONF_PLATFORM_PRI_CAPS="ARM64_LINUX"
           VGCONF_PLATFORM_SEC_CAPS="ARM_LINUX"
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
           valt_load_address_sec_norml="0x58000000"
           valt_load_address_sec_inner="0x38000000"
        fi
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     s390x-linux)
        VGCONF_ARCH_PRI="s390x"
        VGCONF_ARCH_SEC=""
        VGCONF_PLATFORM_PRI_CAPS="S390X_LINUX"
        VGCONF_PLATFORM_SEC_CAPS=""
        # To improve branch prediction hit rate we want to have
        # the generated code close to valgrind (host) code
        valt_load_address_pri_norml="0x800000000"
        valt_load_address_pri_inner="0x810000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     mips32-linux) 
        VGCONF_ARCH_PRI="mips32"
        VGCONF_ARCH_SEC=""
        VGCONF_PLATFORM_PRI_CAPS="MIPS32_LINUX"
        VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x58000000"
        valt_load_address_pri_inner="0x38000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     mips64-linux)
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        if test x$vg_cv_only64bit = xyes; then
            VGCONF_ARCH_PRI="mips64"
            VGCONF_PLATFORM_SEC_CAPS=""
            VGCONF_PLATFORM_PRI_CAPS="MIPS64_LINUX"
            VGCONF_PLATFORM_SEC_CAPS=""
            valt_load_address_pri_norml="0x58000000"
            valt_load_address_pri_inner="0x38000000"
        elif test x$vg_cv_only32bit = xyes; then
            VGCONF_ARCH_PRI="mips32"
            VGCONF_ARCH_SEC=""
            VGCONF_PLATFORM_PRI_CAPS="MIPS32_LINUX"
            VGCONF_PLATFORM_SEC_CAPS=""
            valt_load_address_pri_norml="0x58000000"
            valt_load_address_pri_inner="0x38000000"
        else
            VGCONF_ARCH_PRI="mips64"
            VGCONF_ARCH_SEC="mips32"
            VGCONF_PLATFORM_PRI_CAPS="MIPS64_LINUX"
            VGCONF_PLATFORM_SEC_CAPS="MIPS32_LINUX"
            valt_load_address_pri_norml="0x58000000"
            valt_load_address_pri_inner="0x38000000"
            valt_load_address_sec_norml="0x58000000"
            valt_load_address_sec_inner="0x38000000"
        fi
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     nanomips-linux)
        VGCONF_ARCH_PRI="nanomips"
        VGCONF_ARCH_SEC=""
        VGCONF_PLATFORM_PRI_CAPS="NANOMIPS_LINUX"
        VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x58000000"
        valt_load_address_pri_inner="0x38000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     x86-solaris)
        VGCONF_ARCH_PRI="x86"
        VGCONF_ARCH_SEC=""
        VGCONF_PLATFORM_PRI_CAPS="X86_SOLARIS"
        VGCONF_PLATFORM_SEC_CAPS=""
        valt_load_address_pri_norml="0x58000000"
        valt_load_address_pri_inner="0x38000000"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
     amd64-solaris)
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        if test x$vg_cv_only64bit = xyes; then
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC=""
           VGCONF_PLATFORM_PRI_CAPS="AMD64_SOLARIS"
           VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
        elif test x$vg_cv_only32bit = xyes; then
           VGCONF_ARCH_PRI="x86"
           VGCONF_ARCH_SEC=""
           VGCONF_PLATFORM_PRI_CAPS="X86_SOLARIS"
           VGCONF_PLATFORM_SEC_CAPS=""
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
        else
           VGCONF_ARCH_PRI="amd64"
           VGCONF_ARCH_SEC="x86"
           VGCONF_PLATFORM_PRI_CAPS="AMD64_SOLARIS"
           VGCONF_PLATFORM_SEC_CAPS="X86_SOLARIS"
           valt_load_address_pri_norml="0x58000000"
           valt_load_address_pri_inner="0x38000000"
           valt_load_address_sec_norml="0x58000000"
           valt_load_address_sec_inner="0x38000000"
        fi
        AC_MSG_RESULT([ok (${ARCH_MAX}-${VGCONF_OS})])
        ;;
    *)
        VGCONF_ARCH_PRI="unknown"
        VGCONF_ARCH_SEC="unknown"
	VGCONF_PLATFORM_PRI_CAPS="UNKNOWN"
	VGCONF_PLATFORM_SEC_CAPS="UNKNOWN"
        valt_load_address_pri_norml="0xUNSET"
        valt_load_address_pri_inner="0xUNSET"
        valt_load_address_sec_norml="0xUNSET"
        valt_load_address_sec_inner="0xUNSET"
        AC_MSG_RESULT([no (${ARCH_MAX}-${VGCONF_OS})])
        AC_MSG_ERROR([Valgrind is platform specific. Sorry. Please consider doing a port.])
        ;;
esac

#----------------------------------------------------------------------------

# Set up VGCONF_ARCHS_INCLUDE_<arch>.  Either one or two of these become
# defined.
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_X86,   
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xX86_FREEBSD \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_FREEBSD \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xX86_DARWIN \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_DARWIN \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xX86_SOLARIS \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_SOLARIS )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_AMD64, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_FREEBSD \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_DARWIN \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_SOLARIS )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_PPC32, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xPPC32_LINUX \ 
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xPPC32_LINUX )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_PPC64, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xPPC64BE_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xPPC64LE_LINUX )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_ARM,   
               test x$VGCONF_PLATFORM_PRI_CAPS = xARM_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xARM_LINUX )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_ARM64, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xARM64_LINUX )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_S390X,
               test x$VGCONF_PLATFORM_PRI_CAPS = xS390X_LINUX )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_MIPS32,
               test x$VGCONF_PLATFORM_PRI_CAPS = xMIPS32_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xMIPS32_LINUX )
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_MIPS64,
               test x$VGCONF_PLATFORM_PRI_CAPS = xMIPS64_LINUX ) 
AM_CONDITIONAL(VGCONF_ARCHS_INCLUDE_NANOMIPS,
               test x$VGCONF_PLATFORM_PRI_CAPS = xNANOMIPS_LINUX )

# Set up VGCONF_PLATFORMS_INCLUDE_<platform>.  Either one or two of these
# become defined.
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_X86_LINUX,   
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_AMD64_LINUX, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_PPC32_LINUX, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xPPC32_LINUX \ 
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xPPC32_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_PPC64BE_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xPPC64BE_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_PPC64LE_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xPPC64LE_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_ARM_LINUX, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xARM_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xARM_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_ARM64_LINUX, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xARM64_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_S390X_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xS390X_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xS390X_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_MIPS32_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xMIPS32_LINUX \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xMIPS32_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_MIPS64_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xMIPS64_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_NANOMIPS_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xNANOMIPS_LINUX)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_X86_FREEBSD,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_FREEBSD \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_FREEBSD)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_AMD64_FREEBSD, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_FREEBSD)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_X86_DARWIN,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_DARWIN \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_DARWIN)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_AMD64_DARWIN, 
               test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_DARWIN)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_X86_SOLARIS,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_SOLARIS \
                 -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_SOLARIS)
AM_CONDITIONAL(VGCONF_PLATFORMS_INCLUDE_AMD64_SOLARIS,
               test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_SOLARIS)


# Similarly, set up VGCONF_OS_IS_<os>.  Exactly one of these becomes defined.
# Relies on the assumption that the primary and secondary targets are 
# for the same OS, so therefore only necessary to test the primary.
AM_CONDITIONAL(VGCONF_OS_IS_LINUX,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xPPC32_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xPPC64BE_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xPPC64LE_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xARM_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xARM64_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xS390X_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xMIPS32_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xMIPS64_LINUX \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xNANOMIPS_LINUX)
AM_CONDITIONAL(VGCONF_OS_IS_FREEBSD,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_FREEBSD \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_FREEBSD)
AM_CONDITIONAL(VGCONF_OS_IS_DARWIN,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_DARWIN \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_DARWIN)
AM_CONDITIONAL(VGCONF_OS_IS_SOLARIS,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_SOLARIS \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_SOLARIS)
AM_CONDITIONAL(VGCONF_OS_IS_DARWIN_OR_FREEBSD,
               test x$VGCONF_PLATFORM_PRI_CAPS = xX86_FREEBSD \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_FREEBSD \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xX86_DARWIN \
                 -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_DARWIN)


# Sometimes, in the Makefile.am files, it's useful to know whether or not
# there is a secondary target.
AM_CONDITIONAL(VGCONF_HAVE_PLATFORM_SEC,
               test x$VGCONF_PLATFORM_SEC_CAPS != x)

dnl automake-1.10 does not have AM_COND_IF (added in 1.11), so we supply a
dnl fallback definition
dnl The macro is courtesy of Dave Hart:
dnl   https://lists.gnu.org/archive/html/automake/2010-12/msg00045.html
m4_ifndef([AM_COND_IF], [AC_DEFUN([AM_COND_IF], [
if test -z "$$1_TRUE"; then :
  m4_n([$2])[]dnl
m4_ifval([$3],
[else
  $3
])dnl
fi[]dnl
])])

#----------------------------------------------------------------------------
# Inner Valgrind?
#----------------------------------------------------------------------------

# Check if this should be built as an inner Valgrind, to be run within
# another Valgrind.  Choose the load address accordingly.
AC_SUBST(VALT_LOAD_ADDRESS_PRI)
AC_SUBST(VALT_LOAD_ADDRESS_SEC)
AC_CACHE_CHECK([for use as an inner Valgrind], vg_cv_inner,
   [AC_ARG_ENABLE(inner, 
      [  --enable-inner          enables self-hosting],
      [vg_cv_inner=$enableval],
      [vg_cv_inner=no])])
if test "$vg_cv_inner" = yes; then
    AC_DEFINE([ENABLE_INNER], 1, [configured to run as an inner Valgrind])
    VALT_LOAD_ADDRESS_PRI=$valt_load_address_pri_inner
    VALT_LOAD_ADDRESS_SEC=$valt_load_address_sec_inner
else
    VALT_LOAD_ADDRESS_PRI=$valt_load_address_pri_norml
    VALT_LOAD_ADDRESS_SEC=$valt_load_address_sec_norml
fi

#----------------------------------------------------------------------------
# Undefined behaviour sanitiser
#----------------------------------------------------------------------------
# Check whether we should build with the undefined beahviour sanitiser.

AC_CACHE_CHECK([for using the undefined behaviour sanitiser], vg_cv_ubsan,
   [AC_ARG_ENABLE(ubsan, 
      [  --enable-ubsan          enables the undefined behaviour sanitiser],
      [vg_cv_ubsan=$enableval],
      [vg_cv_ubsan=no])])

#----------------------------------------------------------------------------
# Extra fine-tuning of installation directories
#----------------------------------------------------------------------------
AC_ARG_WITH(tmpdir,
   [  --with-tmpdir=PATH      Specify path for temporary files],
   tmpdir="$withval",
   tmpdir="/tmp")
AC_DEFINE_UNQUOTED(VG_TMPDIR, "$tmpdir", [Temporary files directory])
AC_SUBST(VG_TMPDIR, [$tmpdir])

#----------------------------------------------------------------------------
# Detect xcode path
#----------------------------------------------------------------------------
AM_COND_IF([VGCONF_OS_IS_DARWIN],
[AC_CHECK_PROG([XCRUN], [xcrun], [yes], [no])
AC_MSG_CHECKING([for xcode sdk include path])
AC_ARG_WITH(xcodedir,
   [  --with-xcode-path=PATH      Specify path for xcode sdk includes],
   [xcodedir="$withval"],
   [
      if test "x$XCRUN" != "xno" -a ! -d /usr/include; then
         xcrundir=`xcrun --sdk macosx --show-sdk-path`
         if test -z "$xcrundir"; then
            xcodedir="/usr/include"
         else
            xcodedir="$xcrundir/usr/include"
         fi
      else
         xcodedir="/usr/include"
      fi
   ])
AC_MSG_RESULT([$xcodedir])
AC_DEFINE_UNQUOTED(XCODE_DIR, "$xcodedir", [xcode sdk include directory])
AC_SUBST(XCODE_DIR, [$xcodedir])])

#----------------------------------------------------------------------------
# Where to install gdb scripts, defaults to VG_LIBDIR (pkglibexecdir)
#----------------------------------------------------------------------------
AC_MSG_CHECKING([where gdb scripts are installed])
AC_ARG_WITH(gdbscripts-dir,
   [  --with-gdbscripts-dir=PATH  Specify path to install gdb scripts],
   [gdbscriptsdir=${withval}],
   [gdbscriptsdir=${libexecdir}/valgrind])
AC_MSG_RESULT([$gdbscriptsdir])
if test "x$gdbscriptsdir" != "xno"; then
  AC_SUBST(VG_GDBSCRIPTS_DIR, [$gdbscriptsdir])
  AM_CONDITIONAL(GDBSCRIPTS, true)
else
  AC_SUBST(VG_GDBSCRIPTS_DIR, [])
  AM_CONDITIONAL(GDBSCRIPTS, false)
fi

#----------------------------------------------------------------------------
# Libc and suppressions
#----------------------------------------------------------------------------
# This variable will collect the suppression files to be used.
AC_SUBST(DEFAULT_SUPP)

AC_CHECK_HEADER([features.h])

if test x$ac_cv_header_features_h = xyes; then
  AC_DEFINE([HAVE_HEADER_FEATURES_H], 1,
          [Define to 1 if you have the `features.h' header.])
  rm -f conftest.$ac_ext
  cat <<_ACEOF >conftest.$ac_ext
#include <features.h>
#if defined(__GNU_LIBRARY__) && defined(__GLIBC__) && defined(__GLIBC_MINOR__)
glibc version is: __GLIBC__ __GLIBC_MINOR__
#endif
_ACEOF
  GLIBC_VERSION="`$CPP -P conftest.$ac_ext | $SED -n 's/^glibc version is: //p' | $SED 's/ /./g'`"
fi

# not really a version check
AC_EGREP_CPP([DARWIN_LIBC], [
#include <sys/cdefs.h>
#if defined(__DARWIN_VERS_1050)
  DARWIN_LIBC
#endif
],
GLIBC_VERSION="darwin")

AC_EGREP_CPP([FREEBSD_LIBC], [
#include <sys/cdefs.h>
#if defined(__FreeBSD__)
  FREEBSD_LIBC
#endif
],
GLIBC_VERSION="freebsd")

# not really a version check
AC_EGREP_CPP([BIONIC_LIBC], [
#if defined(__ANDROID__)
  BIONIC_LIBC
#endif
],
GLIBC_VERSION="bionic")

# there is only one version of libc on Solaris
if test x$VGCONF_PLATFORM_PRI_CAPS = xX86_SOLARIS \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_SOLARIS; then
    GLIBC_VERSION="solaris"
fi

# GLIBC_VERSION is empty if a musl libc is used, so use the toolchain tuple
# in this case.
if test x$GLIBC_VERSION = x; then
    if $CC -dumpmachine | grep -q musl; then
        GLIBC_VERSION=musl
    fi
fi

# If this is glibc then figure out the generic (in file) libc.so and
# libpthread.so file paths to use in suppressions. Before 2.34 libpthread
# was a separate library, afterwards it was merged into libc.so and
# the library is called libc.so.6 (before it was libc-2.[0-9]+.so).
# Use this fact to set GLIBC_LIBC_PATH and GLIBC_LIBPTHREAD_PATH.
case ${GLIBC_VERSION} in
2*)
  AC_MSG_CHECKING([whether pthread_create needs libpthread])
  AC_LINK_IFELSE([AC_LANG_CALL([], [pthread_create])],
  [
    AC_MSG_RESULT([no])
    GLIBC_LIBC_PATH="*/lib*/libc.so.6"
    GLIBC_LIBPTHREAD_PATH="$GLIBC_LIBC_PATH"
  ], [
    AC_MSG_RESULT([yes])
    GLIBC_LIBC_PATH="*/lib*/libc-2.*so*"
    GLIBC_LIBPTHREAD_PATH="*/lib*/libpthread-2.*so*"
  ])
  ;;
*)
  AC_MSG_CHECKING([not glibc...])
  AC_MSG_RESULT([${GLIBC_VERSION}])
  ;;
esac

AC_MSG_CHECKING([the glibc version])

case "${GLIBC_VERSION}" in
     2.2)
	AC_MSG_RESULT(${GLIBC_VERSION} family)
        DEFAULT_SUPP="$srcdir/glibc-2.2.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="$srcdir/glibc-2.2-LinuxThreads-helgrind.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-drd.supp ${DEFAULT_SUPP}"
	;;
     2.[[3-6]])
	AC_MSG_RESULT(${GLIBC_VERSION} family)
        DEFAULT_SUPP="$srcdir/glibc-${GLIBC_VERSION}.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-helgrind.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-drd.supp ${DEFAULT_SUPP}"
	;;
     2.[[7-9]])
	AC_MSG_RESULT(${GLIBC_VERSION} family)
	DEFAULT_SUPP="glibc-2.X.supp ${DEFAULT_SUPP}"
	DEFAULT_SUPP="glibc-2.X-helgrind.supp ${DEFAULT_SUPP}"
	DEFAULT_SUPP="glibc-2.X-drd.supp ${DEFAULT_SUPP}"
	;;
     2.10|2.11)
	AC_MSG_RESULT(${GLIBC_VERSION} family)
	AC_DEFINE([GLIBC_MANDATORY_STRLEN_REDIRECT], 1,
		  [Define to 1 if strlen() has been optimized heavily (amd64 glibc >= 2.10)])
	DEFAULT_SUPP="glibc-2.X.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-helgrind.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-drd.supp ${DEFAULT_SUPP}"
	;;
     2.*)
	AC_MSG_RESULT(${GLIBC_VERSION} family)
	AC_DEFINE([GLIBC_MANDATORY_STRLEN_REDIRECT], 1,
		  [Define to 1 if strlen() has been optimized heavily (amd64 glibc >= 2.10)])
	AC_DEFINE([GLIBC_MANDATORY_INDEX_AND_STRLEN_REDIRECT], 1,
		  [Define to 1 if index() and strlen() have been optimized heavily (x86 glibc >= 2.12)])
	DEFAULT_SUPP="glibc-2.X.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-helgrind.supp ${DEFAULT_SUPP}"
        DEFAULT_SUPP="glibc-2.X-drd.supp ${DEFAULT_SUPP}"
	;;
     darwin)
	AC_MSG_RESULT(Darwin)
	AC_DEFINE([DARWIN_LIBC], 1, [Define to 1 if you're using Darwin])
	# DEFAULT_SUPP set by kernel version check above.
	;;
     freebsd)
	AC_MSG_RESULT(FreeBSD)
	AC_DEFINE([FREEBSD_LIBC], 1, [Define to 1 if you're using FreeBSD])
	# DEFAULT_SUPP set by kernel version check above.
	;;
     bionic)
	AC_MSG_RESULT(Bionic)
	AC_DEFINE([BIONIC_LIBC], 1, [Define to 1 if you're using Bionic])
	DEFAULT_SUPP="$srcdir/bionic.supp ${DEFAULT_SUPP}"
	;;
     solaris)
	AC_MSG_RESULT(Solaris)
	# DEFAULT_SUPP set in host_os switch-case above.
	# No other suppression file is used.
	;;
     musl)
	AC_MSG_RESULT(Musl)
	AC_DEFINE([MUSL_LIBC], 1, [Define to 1 if you're using Musl libc])
	DEFAULT_SUPP="$srcdir/musl.supp ${DEFAULT_SUPP}"
	;;
     2.0|2.1|*)
	AC_MSG_RESULT([unsupported version ${GLIBC_VERSION}])
	AC_MSG_ERROR([Valgrind requires glibc version 2.2 or later, uClibc,])
	AC_MSG_ERROR([musl libc, Darwin libc, Bionic libc or Solaris libc])
	;;
esac

AC_SUBST(GLIBC_VERSION)
AC_SUBST(GLIBC_LIBC_PATH)
AC_SUBST(GLIBC_LIBPTHREAD_PATH)


if test "$VGCONF_OS" != "solaris"; then
    # Add default suppressions for the X client libraries.  Make no
    # attempt to detect whether such libraries are installed on the
    # build machine (or even if any X facilities are present); just
    # add the suppressions antidisirregardless.
    DEFAULT_SUPP="$srcdir/xfree-4.supp ${DEFAULT_SUPP}"
    DEFAULT_SUPP="$srcdir/xfree-3.supp ${DEFAULT_SUPP}"
fi


#----------------------------------------------------------------------------
# Platform variants?
#----------------------------------------------------------------------------

# Normally the PLAT = (ARCH, OS) characterisation of the platform is enough.
# But there are times where we need a bit more control.  The motivating
# and currently only case is Android: this is almost identical to
# {x86,arm,mips}-linux, but not quite.  So this introduces the concept of
# platform variant tags, which get passed in the compile as
# -DVGPV_<arch>_<os>_<variant> along with the main -DVGP_<arch>_<os> definition.
#
# In almost all cases, the <variant> bit is "vanilla".  But for Android
# it is "android" instead.
#
# Consequently (eg), plain arm-linux would build with
#
#   -DVGP_arm_linux -DVGPV_arm_linux_vanilla
#
# whilst an Android build would have
#
#   -DVGP_arm_linux -DVGPV_arm_linux_android
#
# Same for x86. The setup of the platform variant is pushed relatively far
# down this file in order that we can inspect any of the variables set above.

# In the normal case ..
VGCONF_PLATVARIANT="vanilla"

# Android ?
if test "$GLIBC_VERSION" = "bionic";
then
   VGCONF_PLATVARIANT="android"
fi

AC_SUBST(VGCONF_PLATVARIANT)


# FIXME: do we also want to define automake variables
# VGCONF_PLATVARIANT_IS_<WHATEVER>, where WHATEVER is (currently)
# VANILLA or ANDROID ?  This would be in the style of VGCONF_ARCHS_INCLUDE,
# VGCONF_PLATFORMS_INCLUDE and VGCONF_OS_IS above?  Could easily enough
# do that.  Problem is that we can't do and-ing in Makefile.am's, but
# that's what we'd need to do to use this, since what we'd want to write
# is something like
#
# VGCONF_PLATFORMS_INCLUDE_ARM_LINUX && VGCONF_PLATVARIANT_IS_ANDROID
#
# Hmm.  Can't think of a nice clean solution to this.

AM_CONDITIONAL(VGCONF_PLATVARIANT_IS_VANILLA,
               test x$VGCONF_PLATVARIANT = xvanilla)
AM_CONDITIONAL(VGCONF_PLATVARIANT_IS_ANDROID,
               test x$VGCONF_PLATVARIANT = xandroid)


#----------------------------------------------------------------------------
# Checking for various library functions and other definitions
#----------------------------------------------------------------------------

# Check for AT_FDCWD

AC_MSG_CHECKING([for AT_FDCWD])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <fcntl.h>
#include <unistd.h>
]], [[
  int a = AT_FDCWD;
]])], [
ac_have_at_fdcwd=yes
AC_MSG_RESULT([yes])
], [
ac_have_at_fdcwd=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL([HAVE_AT_FDCWD], [test x$ac_have_at_fdcwd = xyes])

# Check for stpncpy function definition in string.h
# This explicitly checks with _GNU_SOURCE defined since that is also
# used in the test case (some systems might define it without anyway
# since stpncpy is part of The Open Group Base Specifications Issue 7
# IEEE Std 1003.1-2008.
AC_MSG_CHECKING([for stpncpy])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <string.h>
]], [[
  char *d;
  char *s;
  size_t n = 0;
  char *r = stpncpy(d, s, n);
]])], [
ac_have_gnu_stpncpy=yes
AC_MSG_RESULT([yes])
], [
ac_have_gnu_stpncpy=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL([HAVE_GNU_STPNCPY], [test x$ac_have_gnu_stpncpy = xyes])

# Check for PTRACE_GETREGS

AC_MSG_CHECKING([for PTRACE_GETREGS])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
#include <stddef.h>
#include <sys/ptrace.h>
#include <sys/user.h>
]], [[
  void *p;
  long res = ptrace (PTRACE_GETREGS, 0, p, p);
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTRACE_GETREGS], 1,
          [Define to 1 if you have the `PTRACE_GETREGS' ptrace request.])
], [
AC_MSG_RESULT([no])
])


# Check for CLOCK_MONOTONIC

AC_MSG_CHECKING([for CLOCK_MONOTONIC])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <time.h>
]], [[
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return 0;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_CLOCK_MONOTONIC], 1,
          [Define to 1 if you have the `CLOCK_MONOTONIC' constant.])
], [
AC_MSG_RESULT([no])
])


# Check for ELF32/64_CHDR

AC_CHECK_TYPES([Elf32_Chdr, Elf64_Chdr], [], [], [[#include <elf.h>]])


# Check for PTHREAD_RWLOCK_T

AC_MSG_CHECKING([for pthread_rwlock_t])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <pthread.h>
]], [[
  pthread_rwlock_t rwl;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTHREAD_RWLOCK_T], 1,
          [Define to 1 if you have the `pthread_rwlock_t' type.])
], [
AC_MSG_RESULT([no])
])

# Check for CLOCKID_T

AC_MSG_CHECKING([for clockid_t])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <time.h>
]], [[
  clockid_t c;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_CLOCKID_T], 1,
          [Define to 1 if you have the `clockid_t' type.])
], [
AC_MSG_RESULT([no])
])

# Check for PTHREAD_MUTEX_ADAPTIVE_NP

AC_MSG_CHECKING([for PTHREAD_MUTEX_ADAPTIVE_NP])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <pthread.h>
]], [[
  return (PTHREAD_MUTEX_ADAPTIVE_NP);
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTHREAD_MUTEX_ADAPTIVE_NP], 1,
          [Define to 1 if you have the `PTHREAD_MUTEX_ADAPTIVE_NP' constant.])
], [
AC_MSG_RESULT([no])
])


# Check for PTHREAD_MUTEX_ERRORCHECK_NP

AC_MSG_CHECKING([for PTHREAD_MUTEX_ERRORCHECK_NP])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <pthread.h>
]], [[
  return (PTHREAD_MUTEX_ERRORCHECK_NP);
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTHREAD_MUTEX_ERRORCHECK_NP], 1,
          [Define to 1 if you have the `PTHREAD_MUTEX_ERRORCHECK_NP' constant.])
], [
AC_MSG_RESULT([no])
])


# Check for PTHREAD_MUTEX_RECURSIVE_NP

AC_MSG_CHECKING([for PTHREAD_MUTEX_RECURSIVE_NP])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <pthread.h>
]], [[
  return (PTHREAD_MUTEX_RECURSIVE_NP);
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTHREAD_MUTEX_RECURSIVE_NP], 1,
          [Define to 1 if you have the `PTHREAD_MUTEX_RECURSIVE_NP' constant.])
], [
AC_MSG_RESULT([no])
])


# Check for PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP

AC_MSG_CHECKING([for PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <pthread.h>
]], [[
  pthread_mutex_t m = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
  return 0;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP], 1,
          [Define to 1 if you have the `PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP' constant.])
], [
AC_MSG_RESULT([no])
])


# Check whether pthread_mutex_t has a member called __m_kind.

AC_CHECK_MEMBER([pthread_mutex_t.__m_kind],
	        [AC_DEFINE([HAVE_PTHREAD_MUTEX_T__M_KIND],
		           1,                                   
			   [Define to 1 if pthread_mutex_t has a member called __m_kind.])
                ],
		[],
		[#include <pthread.h>])


# Check whether pthread_mutex_t has a member called __data.__kind.

AC_CHECK_MEMBER([pthread_mutex_t.__data.__kind],
                [AC_DEFINE([HAVE_PTHREAD_MUTEX_T__DATA__KIND],
		          1,
			  [Define to 1 if pthread_mutex_t has a member __data.__kind.])
                ],
		[],
		[#include <pthread.h>])

# Convenience function.  Set flags based on the existing HWCAP entries.
# The AT_HWCAP entries are generated by glibc, and are based on
# functions supported by the hardware/system/libc.
# Subsequent support for whether the capability will actually be utilized
# will also be checked against the compiler capabilities.
# called as
#      AC_HWCAP_CONTAINS_FLAG[hwcap_string_to_match],[VARIABLE_TO_SET]
AC_DEFUN([AC_HWCAP_CONTAINS_FLAG],[
  AUXV_CHECK_FOR=$1
  AC_MSG_CHECKING([if AT_HWCAP contains the $AUXV_CHECK_FOR indicator])
  if env LD_SHOW_AUXV=1 true | grep ^AT_HWCAP | grep -q -w ${AUXV_CHECK_FOR}
  then
    AC_MSG_RESULT([yes])
    AC_SUBST([$2],[yes])
  else
    AC_MSG_RESULT([no])
    AC_SUBST([$2],[])
  fi
])

# gather hardware capabilities. (hardware/kernel/libc)
AC_HWCAP_CONTAINS_FLAG([altivec],[HWCAP_HAS_ALTIVEC])
AC_HWCAP_CONTAINS_FLAG([vsx],[HWCAP_HAS_VSX])
AC_HWCAP_CONTAINS_FLAG([dfp],[HWCAP_HAS_DFP])
AC_HWCAP_CONTAINS_FLAG([arch_2_05],[HWCAP_HAS_ISA_2_05])
AC_HWCAP_CONTAINS_FLAG([arch_2_06],[HWCAP_HAS_ISA_2_06])
AC_HWCAP_CONTAINS_FLAG([arch_2_07],[HWCAP_HAS_ISA_2_07])
AC_HWCAP_CONTAINS_FLAG([arch_3_00],[HWCAP_HAS_ISA_3_00])
AC_HWCAP_CONTAINS_FLAG([arch_3_1],[HWCAP_HAS_ISA_3_1])
AC_HWCAP_CONTAINS_FLAG([htm],[HWCAP_HAS_HTM])
AC_HWCAP_CONTAINS_FLAG([mma],[HWCAP_HAS_MMA])

# ISA Levels
AM_CONDITIONAL(HAS_ISA_2_05, [test x$HWCAP_HAS_ISA_2_05 = xyes])
AM_CONDITIONAL(HAS_ISA_2_06, [test x$HWCAP_HAS_ISA_2_06 = xyes])
# compiler support for isa 2.07 level instructions
AC_MSG_CHECKING([that assembler knows ISA 2.07 instructions ])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  __asm__ __volatile__("mtvsrd 1,2 ");
]])], [
ac_asm_have_isa_2_07=yes
AC_MSG_RESULT([yes])
], [
ac_asm_have_isa_2_07=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(HAS_ISA_2_07, [test x$ac_asm_have_isa_2_07 = xyes \
                               -a x$HWCAP_HAS_ISA_2_07 = xyes])

# altivec (vsx) support.
# does this compiler support -maltivec and does it have the include file
# <altivec.h> ?
AC_MSG_CHECKING([for Altivec support in the compiler ])
safe_CFLAGS=$CFLAGS
CFLAGS="-maltivec -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <altivec.h>
]], [[
  vector unsigned int v;
]])], [
ac_have_altivec=yes
AC_MSG_RESULT([yes])
], [
ac_have_altivec=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
AM_CONDITIONAL([HAS_ALTIVEC], [test x$ac_have_altivec = xyes \
                                 -a x$HWCAP_HAS_ALTIVEC = xyes])

# Check that both: the compiler supports -mvsx and that the assembler
# understands VSX instructions.  If either of those doesn't work,
# conclude that we can't do VSX.
AC_MSG_CHECKING([for VSX compiler flag support])
safe_CFLAGS=$CFLAGS
CFLAGS="-mvsx -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
]])], [
ac_compiler_supports_vsx_flag=yes
AC_MSG_RESULT([yes])
], [
ac_compiler_supports_vsx_flag=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_MSG_CHECKING([for VSX support in the assembler ])
safe_CFLAGS=$CFLAGS
CFLAGS="-mvsx -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <altivec.h>
]], [[
  vector unsigned int v;
  __asm__ __volatile__("xsmaddadp 32, 32, 33" ::: "memory","cc");
]])], [
ac_compiler_supports_vsx=yes
AC_MSG_RESULT([yes])
], [
ac_compiler_supports_vsx=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
AM_CONDITIONAL([HAS_VSX], [test x$ac_compiler_supports_vsx_flag = xyes \
                             -a x$ac_compiler_supports_vsx = xyes \
                             -a x$HWCAP_HAS_VSX = xyes ])

# DFP (Decimal Float)
# The initial DFP support was added in Power 6.  The dcffix instruction
# support was added in Power 7.
AC_MSG_CHECKING([that assembler knows DFP])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  #ifdef __s390__
  __asm__ __volatile__("adtr 1, 2, 3")
  #else
__asm__ __volatile__(".machine power7;\n" \
  "dadd 1, 2, 3;\n" \
  "dcffix 1, 2");
  #endif
]])], [
ac_asm_have_dfp=yes
AC_MSG_RESULT([yes])
], [
ac_asm_have_dfp=no
AC_MSG_RESULT([no])
])
AC_MSG_CHECKING([that compiler knows -mhard-dfp switch])
safe_CFLAGS=$CFLAGS
CFLAGS="-mhard-dfp -Werror"

# The dcffix instruction is Power 7
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  #ifdef __s390__
  __asm__ __volatile__("adtr 1, 2, 3")
  #else
  __asm__ __volatile__(".machine power7;\n" \
  "dadd 1, 2, 3;\n" \
  "dcffix 1, 2");
  #endif
]])], [
ac_compiler_have_dfp=yes
AC_MSG_RESULT([yes])
], [
ac_compiler_have_dfp=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
AM_CONDITIONAL(HAS_DFP, test x$ac_asm_have_dfp = xyes \
                          -a x$ac_compiler_have_dfp = xyes \
                          -a x$HWCAP_HAS_DFP = xyes )

AC_MSG_CHECKING([that compiler knows DFP datatypes])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  _Decimal64 x = 0.0DD;
]])], [
ac_compiler_have_dfp_type=yes
AC_MSG_RESULT([yes])
], [
ac_compiler_have_dfp_type=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(BUILD_DFP_TESTS, test x$ac_compiler_have_dfp_type = xyes \
                                  -a x$HWCAP_HAS_DFP = xyes )


# HTM (Hardware Transactional Memory)
AC_MSG_CHECKING([if compiler accepts the -mhtm flag])
safe_CFLAGS=$CFLAGS
CFLAGS="-mhtm -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  return 0;
]])], [
AC_MSG_RESULT([yes])
ac_compiler_supports_htm=yes
], [
AC_MSG_RESULT([no])
ac_compiler_supports_htm=no
])
CFLAGS=$safe_CFLAGS

AC_MSG_CHECKING([if compiler can find the htm builtins])
safe_CFLAGS=$CFLAGS
CFLAGS="-mhtm -Werror"
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
 ]], [[
   if (__builtin_tbegin (0))
      __builtin_tend (0);
 ]])], [
 AC_MSG_RESULT([yes])
ac_compiler_sees_htm_builtins=yes
 ], [
 AC_MSG_RESULT([no])
ac_compiler_sees_htm_builtins=no
 ])
CFLAGS=$safe_CFLAGS

AM_CONDITIONAL(SUPPORTS_HTM, test x$ac_compiler_supports_htm = xyes \
                               -a x$ac_compiler_sees_htm_builtins = xyes \
                               -a x$HWCAP_HAS_HTM = xyes )

# isa 3.0 checking. (actually 3.0 or newer)
AC_MSG_CHECKING([that assembler knows ISA 3.00 ])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
__asm__ __volatile__ (".machine power9;\n" \
	"cnttzw   1,3; \n" );
]])], [
# guest_ppc_helpers.c needs the HAS_ISA_3_OO to enable copy, paste,
# cpabort support
safe_CFLAGS=$CFLAGS
CFLAGS="-DHAS_ISA_3_00"
ac_asm_have_isa_3_00=yes
AC_MSG_RESULT([yes])
], [
ac_asm_have_isa_3_00=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

# xscvhpdp checking
AC_MSG_CHECKING([that assembler knows xscvhpdp ])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
__asm__ __volatile__ (".machine power9;\n" \
	"xscvhpdp 1,2;\n" );
]])], [
ac_asm_have_xscvhpdp=yes
AC_MSG_RESULT([yes])
], [
ac_asm_have_xscvhpdp=no
AC_MSG_RESULT([no])
])

# darn instruction checking
AC_MSG_CHECKING([that assembler knows darn instruction ])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  __asm__ __volatile__(".machine power9; darn 1,0 ");
]])], [
ac_asm_have_darn_inst=yes
AC_MSG_RESULT([yes])
], [
ac_asm_have_darn_inst=no
AC_MSG_RESULT([no])
])

# isa 3.01 checking
AC_MSG_CHECKING([that assembler knows ISA 3.1 ])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
__asm__ __volatile__ (".machine power10;\n" \
	"brh 1,2;\n ");
]])], [
ac_asm_have_isa_3_1=yes
AC_MSG_RESULT([yes])
], [
ac_asm_have_isa_3_1=no
AC_MSG_RESULT([no])
])


AM_CONDITIONAL(HAS_ISA_3_00, [test x$ac_asm_have_isa_3_00 = xyes \
                             -a x$HWCAP_HAS_ISA_3_00 = xyes])

AM_CONDITIONAL(HAS_XSCVHPDP, [test x$ac_asm_have_xscvhpdp = xyes])
AM_CONDITIONAL(HAS_DARN, [test x$ac_asm_have_darn_inst = xyes])

AM_CONDITIONAL(HAS_ISA_3_1, [test x$ac_asm_have_isa_3_1 = xyes \
                             -a x$HWCAP_HAS_ISA_3_1 = xyes])

# Check for pthread_create@GLIBC2.0
AC_MSG_CHECKING([for pthread_create@GLIBC2.0()])

safe_CFLAGS=$CFLAGS
CFLAGS="-lpthread -Werror"
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
extern int pthread_create_glibc_2_0(void*, const void*,
                                    void *(*)(void*), void*);
__asm__(".symver pthread_create_glibc_2_0, pthread_create@GLIBC_2.0");
]], [[
#ifdef __powerpc__
/*
 * Apparently on PowerPC linking this program succeeds and generates an
 * executable with the undefined symbol pthread_create@GLIBC_2.0.
 */
#error This test does not work properly on PowerPC.
#else
  pthread_create_glibc_2_0(0, 0, 0, 0);
#endif
  return 0;
]])], [
ac_have_pthread_create_glibc_2_0=yes
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_PTHREAD_CREATE_GLIBC_2_0], 1,
          [Define to 1 if you have the `pthread_create@glibc2.0' function.])
], [
ac_have_pthread_create_glibc_2_0=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AM_CONDITIONAL(HAVE_PTHREAD_CREATE_GLIBC_2_0,
	       test x$ac_have_pthread_create_glibc_2_0 = xyes)


# Check for dlinfo RTLD_DI_TLS_MODID
AC_MSG_CHECKING([for dlinfo RTLD_DI_TLS_MODID])

safe_LIBS="$LIBS"
LIBS="-ldl"
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <link.h>
#include <dlfcn.h>
]], [[
  size_t sizes[10000];
  size_t modid_offset;
  (void) dlinfo ((void*)sizes, RTLD_DI_TLS_MODID, &modid_offset);
  return 0;
]])], [
ac_have_dlinfo_rtld_di_tls_modid=yes
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_DLINFO_RTLD_DI_TLS_MODID], 1,
          [Define to 1 if you have a dlinfo that can do RTLD_DI_TLS_MODID.])
], [
ac_have_dlinfo_rtld_di_tls_modid=no
AC_MSG_RESULT([no])
])
LIBS=$safe_LIBS

AM_CONDITIONAL(HAVE_DLINFO_RTLD_DI_TLS_MODID,
	       test x$ac_have_dlinfo_rtld_di_tls_modid = xyes)


# Check for eventfd_t, eventfd() and eventfd_read()
AC_MSG_CHECKING([for eventfd()])

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <sys/eventfd.h>
]], [[
  eventfd_t ev;
  int fd;

  fd = eventfd(5, 0);
  eventfd_read(fd, &ev);
  return 0;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_EVENTFD], 1,
          [Define to 1 if you have the `eventfd' function.])
AC_DEFINE([HAVE_EVENTFD_READ], 1,
          [Define to 1 if you have the `eventfd_read' function.])
], [
AC_MSG_RESULT([no])
])

# Check whether compiler can process #include <thread> without errors
# clang 3.3 cannot process <thread> from e.g.
# gcc (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3

AC_MSG_CHECKING([that C++ compiler can compile C++17 code])
AC_LANG(C++)
safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS=-std=c++17

AC_COMPILE_IFELSE([AC_LANG_SOURCE([
int x;
])],
[
ac_have_cxx_17=yes
AC_MSG_RESULT([yes])
], [
ac_have_cxx_17=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL(HAVE_CXX17, test x$ac_have_cxx_17 = xyes)

AC_MSG_CHECKING([that C++ compiler can include <thread> header file])
AC_LANG(C++)
safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS=-std=c++0x

AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <thread> 
])],
[
ac_cxx_can_include_thread_header=yes
AC_MSG_RESULT([yes])
], [
ac_cxx_can_include_thread_header=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL(CXX_CAN_INCLUDE_THREAD_HEADER, test x$ac_cxx_can_include_thread_header = xyes)

# Check whether compiler can process #include <condition_variable> without errors

AC_MSG_CHECKING([that C++ compiler can include <condition_variable> header file])
AC_LANG(C++)
safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS=-std=c++0x

AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <condition_variable> 
])],
[
ac_cxx_can_include_condition_variable_header=yes
AC_MSG_RESULT([yes])
], [
ac_cxx_can_include_condition_variable_header=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL(CXX_CAN_INCLUDE_CONDITION_VARIABLE_HEADER, test x$ac_cxx_can_include_condition_variable_header = xyes)

# check for std::shared_timed_mutex, this is a C++ 14 feature

AC_MSG_CHECKING([that C++ compiler can use std::shared_timed_mutex])
AC_LANG(C++)
safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="-std=c++1y -pthread"

AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <shared_mutex>
std::shared_timed_mutex test_mutex;
])],
[
ac_cxx_can_use_shared_timed_mutex=yes
AC_MSG_RESULT([yes])
], [
ac_cxx_can_use_shared_timed_mutex=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL(CXX_CAN_USE_SHARED_TIMED_MUTEX, test x$ac_cxx_can_use_shared_timed_mutex = xyes)

# check for std::shared_mutex, this is a C++ 11 feature

AC_MSG_CHECKING([that C++ compiler can use std::timed_mutex])
AC_LANG(C++)
safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="-std=c++0x -pthread"

AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <mutex>
std::timed_mutex test_mutex;
])],
[
ac_cxx_can_use_timed_mutex=yes
AC_MSG_RESULT([yes])
], [
ac_cxx_can_use_timed_mutex=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL(CXX_CAN_USE_TIMED_MUTEX, test x$ac_cxx_can_use_timed_mutex = xyes)

# On aarch64 before glibc 2.20 we would get the kernel user_pt_regs instead
# of the user_regs_struct from sys/user.h. They are structurally the same
# but we get either one or the other.

AC_CHECK_TYPE([struct user_regs_struct],
              [sys_user_has_user_regs=yes], [sys_user_has_user_regs=no],
              [[#include <sys/ptrace.h>]
               [#include <sys/time.h>]
               [#include <sys/user.h>]])
if test "$sys_user_has_user_regs" = "yes"; then
  AC_DEFINE(HAVE_SYS_USER_REGS, 1,
            [Define to 1 if <sys/user.h> defines struct user_regs_struct])
fi

AC_MSG_CHECKING([for __NR_membarrier])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <linux/unistd.h>
]], [[
return __NR_membarrier
]])], [
ac_have_nr_membarrier=yes
AC_MSG_RESULT([yes])
], [
ac_have_nr_membarrier=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(HAVE_NR_MEMBARRIER, [test x$ac_have_nr_membarrier = xyes])

#----------------------------------------------------------------------------
# Checking for supported compiler flags.
#----------------------------------------------------------------------------

case "${host_cpu}" in
    mips*)
        ARCH=$(echo "$CFLAGS" | grep -E -e '-march=@<:@^ @:>@+' -e '\B-mips@<:@^ +@:>@')
        if test -z "$ARCH"; then
          # does this compiler support -march=mips32 (mips32 default) ?
          AC_MSG_CHECKING([if gcc accepts -march=mips32 -mabi=32])

          safe_CFLAGS=$CFLAGS
          CFLAGS="$CFLAGS -mips32 -mabi=32 -Werror"

          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
            return 0;
          ]])], [
          FLAG_M32="-mips32 -mabi=32"
          AC_MSG_RESULT([yes])
          ], [
          FLAG_M32=""
          AC_MSG_RESULT([no])
          ])
          CFLAGS=$safe_CFLAGS

          AC_SUBST(FLAG_M32)


          # does this compiler support -march=mips64r2 (mips64r2 default) ?
          AC_MSG_CHECKING([if gcc accepts -march=mips64r2 -mabi=64])

          safe_CFLAGS=$CFLAGS
          CFLAGS="$CFLAGS -march=mips64r2 -mabi=64 -Werror"

          AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
            return 0;
          ]])], [
          FLAG_M64="-march=mips64r2 -mabi=64"
          AC_MSG_RESULT([yes])
          ], [
          FLAG_M64=""
          AC_MSG_RESULT([no])
          ])
          CFLAGS=$safe_CFLAGS

          AC_SUBST(FLAG_M64)
        fi
        ;;
    nanomips*)
        ;;
    *)
        # does this compiler support -m32 ?
        AC_MSG_CHECKING([if gcc accepts -m32])

        safe_CFLAGS=$CFLAGS
        CFLAGS="${FLAG_32ON64} -m32 -Werror"

        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
          return 0;
        ]])], [
        FLAG_M32="${FLAG_32ON64} -m32"
        AC_MSG_RESULT([yes])
        ], [
        FLAG_M32=""
        AC_MSG_RESULT([no])
        ])
        CFLAGS=$safe_CFLAGS

        AC_SUBST(FLAG_M32)


        # does this compiler support -m64 ?
        AC_MSG_CHECKING([if gcc accepts -m64])

        safe_CFLAGS=$CFLAGS
        CFLAGS="-m64 -Werror"

        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
          return 0;
        ]])], [
        FLAG_M64="-m64"
        AC_MSG_RESULT([yes])
        ], [
        FLAG_M64=""
        AC_MSG_RESULT([no])
        ])
        CFLAGS=$safe_CFLAGS

        AC_SUBST(FLAG_M64)
        ;;
esac


ARCH=$(echo "$CFLAGS" | grep -E -e '-march=@<:@^ @:>@+' -e '\B-mips@<:@^ +@:>@')
if test -z "$ARCH"; then
  # does this compiler support -march=octeon (Cavium OCTEON I Specific) ?
  AC_MSG_CHECKING([if gcc accepts -march=octeon])

  safe_CFLAGS=$CFLAGS
  CFLAGS="$CFLAGS $FLAG_M64 -march=octeon -Werror"

  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    return 0;
  ]])], [
  FLAG_OCTEON="-march=octeon"
  AC_MSG_RESULT([yes])
  ], [
  FLAG_OCTEON=""
  AC_MSG_RESULT([no])
  ])
  CFLAGS=$safe_CFLAGS

  AC_SUBST(FLAG_OCTEON)


  # does this compiler support -march=octeon2 (Cavium OCTEON II Specific) ?
  AC_MSG_CHECKING([if gcc accepts -march=octeon2])

  safe_CFLAGS=$CFLAGS
  CFLAGS="$CFLAGS $FLAG_M64 -march=octeon2 -Werror"

  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    return 0;
  ]])], [
  FLAG_OCTEON2="-march=octeon2"
  AC_MSG_RESULT([yes])
  ], [
  FLAG_OCTEON2=""
  AC_MSG_RESULT([no])
  ])
  CFLAGS=$safe_CFLAGS

  AC_SUBST(FLAG_OCTEON2)
fi


# does this compiler support -mmsa (MIPS MSA ASE) ?
AC_MSG_CHECKING([if gcc accepts -mmsa])

safe_CFLAGS=$CFLAGS
CFLAGS="$CFLAGS -mmsa -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    return 0;
]])], [
FLAG_MSA="-mmsa"
AC_MSG_RESULT([yes])
], [
FLAG_MSA=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(FLAG_MSA)

# Are we compiling for the MIPS64 n32 ABI?
AC_MSG_CHECKING([if gcc is producing mips n32 binaries])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#if !defined(_MIPS_SIM) || (defined(_MIPS_SIM) && (_MIPS_SIM != _ABIN32))
#error NO
#endif
]])], [
VGCONF_ABI=N32
FLAG_M64="-march=mips64r2 -mabi=n32"
AC_MSG_RESULT([yes])
], [
AC_MSG_RESULT([no])
])

# Are we compiling for the MIPS64 n64 ABI?
AC_MSG_CHECKING([if gcc is producing mips n64 binaries])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#if !defined(_MIPS_SIM) || (defined(_MIPS_SIM) && (_MIPS_SIM != _ABI64))
#error NO
#endif
]])], [
VGCONF_ABI=64
AC_MSG_RESULT([yes])
], [
AC_MSG_RESULT([no])
])

# We enter the code block below in the following case:
# Target architecture is set to mips64, the desired abi
# was not specified and the compiler's default abi setting
# is neither n32 nor n64.
# Probe for and set the abi to either n64 or n32, in that order,
# which is required for a mips64 build of valgrind.
if test "$ARCH_MAX" = "mips64" -a "x$VGCONF_ABI" = "x"; then
  safe_CFLAGS=$CFLAGS
  CFLAGS="$CFLAGS -mabi=64 -Werror"
  AC_MSG_CHECKING([if gcc is n64 capable])
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
      return 0;
  ]])], [
  VGCONF_ABI=64
  AC_MSG_RESULT([yes])
  ], [
  AC_MSG_RESULT([no])
  ])
  CFLAGS=$safe_CFLAGS

  if test "x$VGCONF_ABI" = "x"; then
    safe_CFLAGS=$CFLAGS
    CFLAGS="$CFLAGS -mabi=n32 -Werror"
    AC_MSG_CHECKING([if gcc is n32 capable])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
        return 0;
    ]])], [
    VGCONF_ABI=N32
    FLAG_M64="-march=mips64r2 -mabi=n32"
    AC_MSG_RESULT([yes])
    ], [
    AC_MSG_RESULT([no])
    ])
    CFLAGS=$safe_CFLAGS
  fi
fi

AM_CONDITIONAL([VGCONF_HAVE_ABI],
               [test x$VGCONF_ABI != x])
AC_SUBST(VGCONF_ABI)


# does this compiler support -mmmx ?
AC_MSG_CHECKING([if gcc accepts -mmmx])

safe_CFLAGS=$CFLAGS
CFLAGS="-mmmx -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
FLAG_MMMX="-mmmx"
AC_MSG_RESULT([yes])
], [
FLAG_MMMX=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(FLAG_MMMX)


# does this compiler support -msse ?
AC_MSG_CHECKING([if gcc accepts -msse])

safe_CFLAGS=$CFLAGS
CFLAGS="-msse -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
FLAG_MSSE="-msse"
AC_MSG_RESULT([yes])
], [
FLAG_MSSE=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(FLAG_MSSE)


# does this compiler support -mpreferred-stack-boundary=2 when
# generating code for a 32-bit target?  Note that we only care about
# this when generating code for (32-bit) x86, so if the compiler
# doesn't recognise -m32 it's no big deal.  We'll just get code for
# the Memcheck and other helper functions, that is a bit slower than
# it could be, on x86; and no difference at all on any other platform.
AC_MSG_CHECKING([if gcc accepts -mpreferred-stack-boundary=2 -m32])

safe_CFLAGS=$CFLAGS
CFLAGS="-mpreferred-stack-boundary=2 -m32 -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
PREFERRED_STACK_BOUNDARY_2="-mpreferred-stack-boundary=2"
AC_MSG_RESULT([yes])
], [
PREFERRED_STACK_BOUNDARY_2=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(PREFERRED_STACK_BOUNDARY_2)


# does this compiler support -mlong-double-128 ?
AC_MSG_CHECKING([if gcc accepts -mlong-double-128])
safe_CFLAGS=$CFLAGS
CFLAGS="-mlong-double-128 -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
ac_compiler_supports_mlong_double_128=yes
AC_MSG_RESULT([yes])
], [
ac_compiler_supports_mlong_double_128=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
AM_CONDITIONAL(HAS_MLONG_DOUBLE_128, test x$ac_compiler_supports_mlong_double_128 = xyes)
FLAG_MLONG_DOUBLE_128="-mlong-double-128"
AC_SUBST(FLAG_MLONG_DOUBLE_128)

# does this toolchain support lto ?
# Not checked for if --enable-lto=no was given, or if LTO_AR or LTO_RANLIG
# are not defined
# If not enable-lto=* arg is provided, default to no, as  lto builds are
# a lot slower, and so not appropriate for Valgrind developments.
# --enable-lto=yes should be used by distro packagers.
AC_CACHE_CHECK([for using the link time optimisation], vg_cv_lto,
   [AC_ARG_ENABLE(lto,
      [  --enable-lto          enables building with link time optimisation],
      [vg_cv_lto=$enableval],
      [vg_cv_lto=no])])

if test "x${vg_cv_lto}" != "xno" -a "x${LTO_AR}" != "x" -a "x${LTO_RANLIB}" != "x"; then
AC_MSG_CHECKING([if toolchain accepts lto])
safe_CFLAGS=$CFLAGS
TEST_LTO_CFLAGS="-flto -flto-partition=one -fuse-linker-plugin"
# Note : using 'one' partition is giving a slightly smaller/faster memcheck
# and ld/lto-trans1 still needs a reasonable memory (about 0.5GB) when linking.
CFLAGS="$TEST_LTO_CFLAGS -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  extern void somefun(void);
  somefun();
  return 0;
]])], [
LTO_CFLAGS=$TEST_LTO_CFLAGS
AC_MSG_RESULT([yes])
], [
LTO_CFLAGS=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
fi

AC_SUBST(LTO_CFLAGS)

# if we could not compile with lto args, or lto was disabled,
# then set LTO_AR/LTO_RANLIB to the non lto values
# define in config.h ENABLE_LTO (not needed by the code currently, but
# this guarantees we recompile everything if we re-configure and rebuild
# in a build dir previously build with another value of --enable-lto
if test "x${LTO_CFLAGS}" = "x"; then
   LTO_AR=${AR}
   LTO_RANLIB=${RANLIB}
   vg_cv_lto=no
else
   vg_cv_lto=yes
   AC_DEFINE([ENABLE_LTO], 1, [configured to build with lto link time optimisation])
fi

# Convenience function to check whether GCC supports a particular
# warning option. Takes two arguments,
# first the warning flag name to check (without -W), then the
# substitution name to set with -Wno-warning-flag if the flag exists,
# or the empty string if the compiler doesn't accept the flag. Note
# that checking is done against the warning flag itself, but the
# substitution is then done to cancel the warning flag.
AC_DEFUN([AC_GCC_WARNING_SUBST_NO],[
  AC_MSG_CHECKING([if gcc accepts -W$1])
  safe_CFLAGS=$CFLAGS
  CFLAGS="-W$1 -Werror"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[;]])], [
  AC_SUBST([$2], [-Wno-$1])
  AC_MSG_RESULT([yes])], [
  AC_SUBST([$2], [])
  AC_MSG_RESULT([no])])
  CFLAGS=$safe_CFLAGS
])

# A variation of the above for arguments that
# take a value
AC_DEFUN([AC_GCC_WARNING_SUBST_NO_VAL],[
  AC_MSG_CHECKING([if gcc accepts -W$1=$2])
  safe_CFLAGS=$CFLAGS
  CFLAGS="-W$1=$2 -Werror"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[;]])], [
  AC_SUBST([$3], [-Wno-$1])
  AC_MSG_RESULT([yes])], [
  AC_SUBST([$3], [])
  AC_MSG_RESULT([no])])
  CFLAGS=$safe_CFLAGS
])

# Convenience function. Like AC_GCC_WARNING_SUBST_NO, except it substitutes
# -W$1  (instead of -Wno-$1).
AC_DEFUN([AC_GCC_WARNING_SUBST],[
  AC_MSG_CHECKING([if gcc accepts -W$1])
  safe_CFLAGS=$CFLAGS
  CFLAGS="-W$1 -Werror"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[;]])], [
  AC_SUBST([$2], [-W$1])
  AC_MSG_RESULT([yes])], [
  AC_SUBST([$2], [])
  AC_MSG_RESULT([no])])
  CFLAGS=$safe_CFLAGS
])

AC_GCC_WARNING_SUBST_NO([memset-transposed-args], [FLAG_W_NO_MEMSET_TRANSPOSED_ARGS])
AC_GCC_WARNING_SUBST_NO([nonnull], [FLAG_W_NO_NONNULL])
AC_GCC_WARNING_SUBST_NO([overflow], [FLAG_W_NO_OVERFLOW])
AC_GCC_WARNING_SUBST_NO([pointer-sign], [FLAG_W_NO_POINTER_SIGN])
AC_GCC_WARNING_SUBST_NO([uninitialized], [FLAG_W_NO_UNINITIALIZED])
AC_GCC_WARNING_SUBST_NO([maybe-uninitialized], [FLAG_W_NO_MAYBE_UNINITIALIZED])
AC_GCC_WARNING_SUBST_NO([unused-function], [FLAG_W_NO_UNUSED_FUNCTION])
AC_GCC_WARNING_SUBST_NO([static-local-in-inline], [FLAG_W_NO_STATIC_LOCAL_IN_INLINE])
AC_GCC_WARNING_SUBST_NO([mismatched-new-delete], [FLAG_W_NO_MISMATCHED_NEW_DELETE])
AC_GCC_WARNING_SUBST_NO([infinite-recursion], [FLAG_W_NO_INFINITE_RECURSION])
AC_GCC_WARNING_SUBST_NO([expansion-to-defined], [FLAG_W_NO_EXPANSION_TO_DEFINED])
AC_GCC_WARNING_SUBST_NO([unused-variable], [FLAG_W_NO_UNUSED_VARIABLE])
AC_GCC_WARNING_SUBST_NO([unused-but-set-variable], [FLAG_W_NO_UNUSED_BUT_SET_VARIABLE])
AC_GCC_WARNING_SUBST_NO([non-power-of-two-alignment], [FLAG_W_NO_NON_POWER_OF_TWO_ALIGNMENT])
AC_GCC_WARNING_SUBST_NO([sign-compare], [FLAG_W_NO_SIGN_COMPARE])
AC_GCC_WARNING_SUBST_NO([stringop-overflow], [FLAG_W_NO_STRINGOP_OVERFLOW])
AC_GCC_WARNING_SUBST_NO([stringop-overread], [FLAG_W_NO_STRINGOP_OVERREAD])
AC_GCC_WARNING_SUBST_NO([stringop-truncation], [FLAG_W_NO_STRINGOP_TRUNCATION])
AC_GCC_WARNING_SUBST_NO([format-overflow], [FLAG_W_NO_FORMAT_OVERFLOW])
AC_GCC_WARNING_SUBST_NO([use-after-free], [FLAG_W_NO_USE_AFTER_FREE])
AC_GCC_WARNING_SUBST_NO([free-nonheap-object], [FLAG_W_NO_FREE_NONHEAP_OBJECT])
AC_GCC_WARNING_SUBST_NO([fortify-source], [FLAG_W_NO_FORTIFY_SOURCE])
AC_GCC_WARNING_SUBST_NO([builtin-memcpy-chk-size], [FLAG_W_NO_BUILTIN_MEMCPY_CHK_SIZE])
AC_GCC_WARNING_SUBST_NO([incompatible-pointer-types-discards-qualifiers], [FLAG_W_NO_INCOMPATIBLE_POINTER_TYPES_DISCARDS_QUALIFIERS])
AC_GCC_WARNING_SUBST_NO([suspicious-bzero], [FLAG_W_NO_SUSPICIOUS_BZERO])
AC_GCC_WARNING_SUBST_NO([attributes], [FLAG_W_NO_ATTRIBUTES])

AC_GCC_WARNING_SUBST_NO_VAL([alloc-size-larger-than], [1677216], [FLAG_W_NO_ALLOC_SIZE_LARGER_THAN])

AC_GCC_WARNING_SUBST([write-strings], [FLAG_W_WRITE_STRINGS])
AC_GCC_WARNING_SUBST([empty-body], [FLAG_W_EMPTY_BODY])
AC_GCC_WARNING_SUBST([format], [FLAG_W_FORMAT])
AC_GCC_WARNING_SUBST([format-signedness], [FLAG_W_FORMAT_SIGNEDNESS])
AC_GCC_WARNING_SUBST([cast-qual], [FLAG_W_CAST_QUAL])
AC_GCC_WARNING_SUBST([old-style-declaration], [FLAG_W_OLD_STYLE_DECLARATION])
AC_GCC_WARNING_SUBST([ignored-qualifiers], [FLAG_W_IGNORED_QUALIFIERS])
AC_GCC_WARNING_SUBST([missing-parameter-type], [FLAG_W_MISSING_PARAMETER_TYPE])
AC_GCC_WARNING_SUBST([logical-op], [FLAG_W_LOGICAL_OP])
AC_GCC_WARNING_SUBST([enum-conversion], [FLAG_W_ENUM_CONVERSION])
AC_GCC_WARNING_SUBST([implicit-fallthrough=2], [FLAG_W_IMPLICIT_FALLTHROUGH])

# Does this compiler support -Wformat-security ?
# Special handling is needed, because certain GCC versions require -Wformat
# being present if -Wformat-security is given. Otherwise a warning is issued.
# However, AC_GCC_WARNING_SUBST will stick in -Werror (see r15323 for rationale).
# And with that the warning will be turned into an error with the result
# that -Wformat-security is believed to be unsupported when in fact it is.
AC_MSG_CHECKING([if gcc accepts -Wformat-security])
safe_CFLAGS=$CFLAGS
CFLAGS="-Wformat -Wformat-security -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[;]])], [
AC_SUBST([FLAG_W_FORMAT_SECURITY], [-Wformat-security])
AC_MSG_RESULT([yes])], [
AC_SUBST([FLAG_W_FORMAT_SECURITY], [])
AC_MSG_RESULT([no])])
CFLAGS=$safe_CFLAGS

# does this compiler support -Wextra or the older -W ?

AC_MSG_CHECKING([if gcc accepts -Wextra or -W])

safe_CFLAGS=$CFLAGS
CFLAGS="-Wextra -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
AC_SUBST([FLAG_W_EXTRA], [-Wextra])
AC_MSG_RESULT([-Wextra])
], [
  CFLAGS="-W -Werror"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[ ]], [[
    return 0;
  ]])], [
  AC_SUBST([FLAG_W_EXTRA], [-W])
  AC_MSG_RESULT([-W])
  ], [
  AC_SUBST([FLAG_W_EXTRA], [])
  AC_MSG_RESULT([not supported])
  ])
])
CFLAGS=$safe_CFLAGS

# On ARM we do not want to pass -Wcast-align as that produces loads
# of warnings. GCC is just being conservative. See here:
# https://gcc.gnu.org/bugzilla/show_bug.cgi?id=65459#c4
if test "X$VGCONF_ARCH_PRI" = "Xarm"; then
  AC_SUBST([FLAG_W_CAST_ALIGN], [""])
else
  AC_SUBST([FLAG_W_CAST_ALIGN], [-Wcast-align])
fi

# does this compiler support -faligned-new ?
AC_MSG_CHECKING([if g++ accepts -faligned-new])

safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="-faligned-new -Werror"

AC_LANG(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
FLAG_FALIGNED_NEW="-faligned-new"
AC_MSG_RESULT([yes])
], [
FLAG_FALIGNED_NEW=""
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AC_SUBST(FLAG_FALIGNED_NEW)

# does this compiler support -fsized-deallocation ?
AC_MSG_CHECKING([if g++ accepts -fsized-deallocation])

safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="-fsized-deallocation -Werror"

AC_LANG(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
FLAG_FSIZED_DEALLOCATION="-fsized-deallocation"
ac_have_sized_deallocation=yes
AC_MSG_RESULT([yes])
], [
FLAG_FSIZED_DEALLOCATION=""
ac_have_sized_deallocation=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AC_SUBST(FLAG_FSIZED_DEALLOCATION)
AM_CONDITIONAL([HAVE_FSIZED_DEALLOCATION], [test x$ac_have_sized_deallocation = xyes])

# does this compiler support C++17 aligned new/delete?
AC_MSG_CHECKING([if g++ supports aligned new and delete])

safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="-std=c++17"

AC_LANG(C++)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <cstdlib>
#include <new>
]], [[
  operator delete(nullptr, std::align_val_t(64U));
]])], [
ac_have_aligned_cxx_alloc=yes
AC_MSG_RESULT([yes])
], [
ac_have_aligned_cxx_alloc=no
AC_MSG_RESULT([no])
])
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL([HAVE_ALIGNED_CXX_ALLOC], [test x$ac_have_aligned_cxx_alloc = xyes])

# does this compiler support -fno-stack-protector ?
AC_MSG_CHECKING([if gcc accepts -fno-stack-protector])

safe_CFLAGS=$CFLAGS
CFLAGS="-fno-stack-protector -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
no_stack_protector=yes
FLAG_FNO_STACK_PROTECTOR="-fno-stack-protector"
AC_MSG_RESULT([yes])
], [
no_stack_protector=no
FLAG_FNO_STACK_PROTECTOR=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(FLAG_FNO_STACK_PROTECTOR)

# does this compiler support -finline-functions ?
AC_MSG_CHECKING([if gcc accepts -finline-functions])

safe_CFLAGS=$CFLAGS
CFLAGS="-finline-functions -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
inline_functions=yes
FLAG_FINLINE_FUNCTIONS="-finline-functions"
AC_MSG_RESULT([yes])
], [
inline_functions=no
FLAG_FINLINE_FUNCTIONS=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(FLAG_FINLINE_FUNCTIONS)

# Does GCC support disabling Identical Code Folding?
# We want to disabled Identical Code Folding for the
# tools preload shared objects to get better backraces.
# For GCC 5.1+ -fipa-icf is enabled by default at -O2.
# "The optimization reduces code size and may disturb
#  unwind stacks by replacing a function by equivalent
#  one with a different name."
AC_MSG_CHECKING([if gcc accepts -fno-ipa-icf])

safe_CFLAGS=$CFLAGS
CFLAGS="-fno-ipa-icf -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
no_ipa_icf=yes
FLAG_FNO_IPA_ICF="-fno-ipa-icf"
AC_MSG_RESULT([yes])
], [
no_ipa_icf=no
FLAG_FNO_IPA_ICF=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_SUBST(FLAG_FNO_IPA_ICF)


# Does this compiler support -fsanitize=undefined. This is true for
# GCC 4.9 and newer. However, the undefined behaviour sanitiser in GCC 5.1
# also checks for alignment violations on memory accesses which the valgrind
# code base is sprinkled (if not littered) with. As those alignment issues
# don't pose a problem we want to suppress warnings about them.
# In GCC 5.1 this can be done by passing -fno-sanitize=alignment. Earlier
# GCCs do not support that.
#
# Only checked for if --enable-ubsan was given.
if test "x${vg_cv_ubsan}" = "xyes"; then
AC_MSG_CHECKING([if gcc accepts -fsanitize=undefined -fno-sanitize=alignment])
safe_CFLAGS=$CFLAGS
CFLAGS="-fsanitize=undefined -fno-sanitize=alignment -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
FLAG_FSANITIZE="-fsanitize=undefined -fno-sanitize=alignment"
LIB_UBSAN="-static-libubsan"
AC_MSG_RESULT([yes])
], [
CFLAGS="-fsanitize=undefined -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return 0;
]])], [
FLAG_FSANITIZE="-fsanitize=undefined"
LIB_UBSAN="-static-libubsan"
AC_MSG_RESULT([yes])
], [
FLAG_FSANITIZE=""
LIB_UBSAN=""
AC_MSG_RESULT([no])
])
])
CFLAGS=$safe_CFLAGS
AC_SUBST(FLAG_FSANITIZE)
AC_SUBST(LIB_UBSAN)
fi
# does this compiler support --param inline-unit-growth=... ?

AC_MSG_CHECKING([if gcc accepts --param inline-unit-growth])

safe_CFLAGS=$CFLAGS
CFLAGS="--param inline-unit-growth=900 -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
AC_SUBST([FLAG_UNLIMITED_INLINE_UNIT_GROWTH],
         ["--param inline-unit-growth=900"])
AC_MSG_RESULT([yes])
], [
AC_SUBST([FLAG_UNLIMITED_INLINE_UNIT_GROWTH], [""])
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS


# does this compiler support -gdwarf-4 -fdebug-types-section ?

AC_MSG_CHECKING([if gcc accepts -gdwarf-4 -fdebug-types-section])

safe_CFLAGS=$CFLAGS
CFLAGS="-gdwarf-4 -fdebug-types-section -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
ac_have_dwarf4=yes
AC_MSG_RESULT([yes])
], [
ac_have_dwarf4=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(DWARF4, test x$ac_have_dwarf4 = xyes)
CFLAGS=$safe_CFLAGS


# does this compiler support -g -gz=zlib ?

AC_MSG_CHECKING([if gcc accepts -g -gz=zlib])

safe_CFLAGS=$CFLAGS
CFLAGS="-g -gz=zlib"

AC_LINK_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
ac_have_gz_zlib=yes
AC_MSG_RESULT([yes])
], [
ac_have_gz_zlib=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(GZ_ZLIB, test x$ac_have_gz_zlib = xyes)
CFLAGS=$safe_CFLAGS


# does this compiler support -g -gz=zlib-gnu ?

AC_MSG_CHECKING([if gcc accepts -g -gz=zlib-gnu])

safe_CFLAGS=$CFLAGS
CFLAGS="-g -gz=zlib-gnu"

AC_LINK_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
ac_have_gz_zlib_gnu=yes
AC_MSG_RESULT([yes])
], [
ac_have_gz_zlib_gnu=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(GZ_ZLIB_GNU, test x$ac_have_gz_zlib_gnu = xyes)
CFLAGS=$safe_CFLAGS


# does this compiler support nested functions ?

AC_MSG_CHECKING([if gcc accepts nested functions])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  int foo() { return 1; }
  return foo();
]])], [
ac_have_nested_functions=yes
AC_MSG_RESULT([yes])
], [
ac_have_nested_functions=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL([HAVE_NESTED_FUNCTIONS], [test x$ac_have_nested_functions = xyes])


# does this compiler support the 'p' constraint in ASM statements ?

AC_MSG_CHECKING([if gcc accepts the 'p' constraint in asm statements])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
   char *p;
   __asm__ __volatile__ ("movdqa (%0),%%xmm6\n" : "=p" (p));
]])], [
ac_have_asm_constraint_p=yes
AC_MSG_RESULT([yes])
], [
ac_have_asm_constraint_p=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL([HAVE_ASM_CONSTRAINT_P], [test x$ac_have_asm_constraint_p = xyes])


# Does this compiler and linker support -pie?
# Some compilers actually do not support -pie and report its usage
# as an error. We need to check if it is safe to use it first.

AC_MSG_CHECKING([if gcc accepts -pie])

safe_CFLAGS=$CFLAGS
CFLAGS="-pie"

AC_LINK_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
AC_SUBST([FLAG_PIE], ["-pie"])
AC_MSG_RESULT([yes])
], [
AC_SUBST([FLAG_PIE], [""])
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AC_MSG_CHECKING([if gcc accepts -ansi])

safe_CFLAGS=$CFLAGS
CFLAGS="-ansi"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
   return 0;
]])], [
ac_have_ansi=yes
AC_MSG_RESULT([yes])
], [
ac_have_ansi=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL([HAVE_ANSI], [test x$ac_have_ansi = xyes])

CFLAGS=$safe_CFLAGS


# Does this compiler support -no-pie?
# On Ubuntu 16.10+, gcc produces position independent executables (PIE) by
# default. However this gets in the way with some tests, we use -no-pie
# for these.

AC_MSG_CHECKING([if gcc accepts -no-pie])

safe_CFLAGS=$CFLAGS
CFLAGS="-no-pie -Werror"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[ ]], [[
  return 0;
]])], [
AC_SUBST([FLAG_NO_PIE], ["-no-pie"])
AC_MSG_RESULT([yes])
], [
AC_SUBST([FLAG_NO_PIE], [""])
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS


# We want to use use the -Ttext-segment option to the linker.
# GNU (bfd) ld supports this directly. Newer GNU gold linkers
# support it as an alias of -Ttext. Sadly GNU (bfd) ld's -Ttext
# semantics are NOT what we want (GNU gold -Ttext is fine).
#
# For GNU (bfd) ld -Ttext-segment chooses the base at which ELF headers
# will reside. -Ttext aligns just the .text section start (but not any
# other section).
#
# LLVM ld.lld 10.0 changed the semantics of its -Ttext. See "Breaking changes"
# in https://releases.llvm.org/10.0.0/tools/lld/docs/ReleaseNotes.html
# The --image-base option (since version 6.0?) provides the semantics needed.
# -Ttext-segment generates an error, but -Ttext now more closely
# follows the GNU (bfd) ld's -Ttext.
#
# So test first for --image-base support, and if that fails then
# for -Ttext-segment which is supported by all bfd ld versions
# and use that if it exists. If it doesn't exist it must be an older
# version of gold and we can fall back to using -Ttext which has the
# right semantics.

safe_CFLAGS=$CFLAGS
AC_MSG_CHECKING([if the linker accepts -Wl,--image-base])

CFLAGS="-static -nodefaultlibs -nostartfiles -Wl,--image-base=$valt_load_address_pri_norml -Werror"

AC_LINK_IFELSE(
[AC_LANG_SOURCE([int _start () { return 0; }])],
[
  linker_using_t_text="no"
  AC_SUBST([FLAG_T_TEXT], ["--image-base"])
  AC_MSG_RESULT([yes])
], [
  AC_MSG_RESULT([no])

  AC_MSG_CHECKING([if the linker accepts -Wl,-Ttext-segment])

  CFLAGS="-static -nodefaultlibs -nostartfiles -Wl,-Ttext-segment=$valt_load_address_pri_norml -Werror"

  AC_LINK_IFELSE(
  [AC_LANG_SOURCE([int _start () { return 0; }])],
  [
    linker_using_t_text="no"
    AC_SUBST([FLAG_T_TEXT], ["-Ttext-segment"])
    AC_MSG_RESULT([yes])
  ], [
    linker_using_t_text="yes"
    AC_SUBST([FLAG_T_TEXT], ["-Ttext"])
    AC_MSG_RESULT([no])
  ])
])

CFLAGS=$safe_CFLAGS

# If the linker only supports -Ttext (not -Ttext-segment or --image-base) then we will
# have to strip any build-id ELF NOTEs from the statically linked tools.
# Otherwise the build-id NOTE might end up at the default load address.
# (Pedantically if the linker is gold then -Ttext is fine, but newer
# gold versions also support -Ttext-segment. So just assume that unless
# we can use -Ttext-segment we need to strip the build-id NOTEs.
if test "x${linker_using_t_text}" = "xyes"; then
AC_MSG_NOTICE([ld -Ttext used, need to strip build-id NOTEs.])
# does the linker support -Wl,--build-id=none ?  Note, it's
# important that we test indirectly via whichever C compiler
# is selected, rather than testing /usr/bin/ld or whatever
# directly.
AC_MSG_CHECKING([if the linker accepts -Wl,--build-id=none])
safe_CFLAGS=$CFLAGS
CFLAGS="-Wl,--build-id=none -Werror"

AC_LINK_IFELSE(
[AC_LANG_PROGRAM([ ], [return 0;])],
[
  AC_SUBST([FLAG_NO_BUILD_ID], ["-Wl,--build-id=none"])
  AC_MSG_RESULT([yes])
], [
  AC_SUBST([FLAG_NO_BUILD_ID], [""])
  AC_MSG_RESULT([no])
])
else
AC_MSG_NOTICE([ld --image-base or -Ttext-segment used, no need to strip build-id NOTEs.])
AC_SUBST([FLAG_NO_BUILD_ID], [""])
fi
CFLAGS=$safe_CFLAGS

# On s390x, if the linker supports -Wl,--s390-pgste, then we build the
# tools with that flag.  This enables running programs that need it, such
# as qemu-kvm.
if test x$VGCONF_PLATFORM_PRI_CAPS = xS390X_LINUX; then
AC_MSG_CHECKING([if the linker accepts -Wl,--s390-pgste])
safe_CFLAGS=$CFLAGS
CFLAGS="-Wl,--s390-pgste"

AC_LINK_IFELSE(
[AC_LANG_PROGRAM([ ], [return 0;])],
[
  AC_SUBST([FLAG_S390_PGSTE], ["-Wl,--s390-pgste"])
  AC_MSG_RESULT([yes])
], [
  AC_SUBST([FLAG_S390_PGSTE], [""])
  AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
fi

# does the ppc assembler support "mtocrf" et al?
AC_MSG_CHECKING([if ppc32/64 as supports mtocrf/mfocrf])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
__asm__ __volatile__("mtocrf 4,0");
__asm__ __volatile__("mfocrf 0,4");
]])], [
ac_have_as_ppc_mftocrf=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_ppc_mftocrf=no
AC_MSG_RESULT([no])
])
if test x$ac_have_as_ppc_mftocrf = xyes ; then
  AC_DEFINE(HAVE_AS_PPC_MFTOCRF, 1, [Define to 1 if as supports mtocrf/mfocrf.])
fi


# does the ppc assembler support "lfdp" and other phased out floating point insns?
AC_MSG_CHECKING([if ppc32/64 asm supports phased out floating point instructions])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { typedef struct {
      double hi;
      double lo;
     } dbl_pair_t;
     dbl_pair_t dbl_pair[3];
     __asm__ volatile ("lfdp 10, %0"::"m" (dbl_pair[0]));
   } while (0)
]])], [
ac_have_as_ppc_fpPO=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_ppc_fpPO=no
AC_MSG_RESULT([no])
])
if test x$ac_have_as_ppc_fpPO = xyes ; then
  AC_DEFINE(HAVE_AS_PPC_FPPO, 1, [Define to 1 if as supports floating point phased out category.])
fi


# does the amd64 assembler understand "fxsave64" and "fxrstor64"?
AC_MSG_CHECKING([if amd64 assembler supports fxsave64/fxrstor64])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
void* p;
asm __volatile__("fxsave64 (%0)" : : "r" (p) : "memory" );
asm __volatile__("fxrstor64 (%0)" : : "r" (p) : "memory" );
]])], [
ac_have_as_amd64_fxsave64=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_amd64_fxsave64=no
AC_MSG_RESULT([no])
])
if test x$ac_have_as_amd64_fxsave64 = xyes ; then
  AC_DEFINE(HAVE_AS_AMD64_FXSAVE64, 1, [Define to 1 if as supports fxsave64/fxrstor64.])
fi

# does the x86/amd64 assembler understand SSE3 instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_SSE3_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks SSE3])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { long long int x; 
     __asm__ __volatile__("fisttpq (%0)" : :"r"(&x) ); } 
  while (0)
]])], [
ac_have_as_sse3=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_sse3=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_SSE3_TESTS, test x$ac_have_as_sse3 = xyes)


# Ditto for SSSE3 instructions (note extra S)
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_SSSE3_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks SSSE3])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -msse -Werror"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { long long int x; 
   __asm__ __volatile__(
      "pabsb (%0),%%xmm7" : : "r"(&x) : "xmm7" ); }
  while (0)
]])], [
ac_have_as_ssse3=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_ssse3=no
AC_MSG_RESULT([no])
])
CFLAGS="$save_CFLAGS"

AM_CONDITIONAL(BUILD_SSSE3_TESTS, test x$ac_have_as_ssse3 = xyes)


# does the x86/amd64 assembler understand the PCLMULQDQ instruction?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_PCLMULQDQ_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler supports 'pclmulqdq'])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
   __asm__ __volatile__(
      "pclmulqdq \$17,%%xmm6,%%xmm7" : : : "xmm6", "xmm7" ); }
  while (0)
]])], [
ac_have_as_pclmulqdq=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_pclmulqdq=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_PCLMULQDQ_TESTS, test x$ac_have_as_pclmulqdq = xyes)


# does the x86/amd64 assembler understand the VPCLMULQDQ instruction?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_VPCLMULQDQ_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler supports 'vpclmulqdq'])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
      /*
       * Carry-less multiplication of xmm1 with xmm2 and store the result in
       * xmm3. The immediate is used to determine which quadwords of xmm1 and
       * xmm2 should be used.
       */
   __asm__ __volatile__(
      "vpclmulqdq \$0,%%xmm1,%%xmm2,%%xmm3" : : : );
  } while (0)
]])], [
ac_have_as_vpclmulqdq=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_vpclmulqdq=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_VPCLMULQDQ_TESTS, test x$ac_have_as_vpclmulqdq = xyes)


# does the x86/amd64 assembler understand FMA4 instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_AFM4_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler supports FMA4 'vfmaddpd'])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
   __asm__ __volatile__(
      "vfmaddpd %%xmm7,%%xmm8,%%xmm6,%%xmm9" : : : );
  } while (0)
]])], [
ac_have_as_vfmaddpd=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_vfmaddpd=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_FMA4_TESTS, test x$ac_have_as_vfmaddpd = xyes)


# does the x86/amd64 assembler understand the LZCNT instruction?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_LZCNT_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler supports 'lzcnt'])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { 		 
      __asm__ __volatile__("lzcnt %%rax,%%rax" : : : "rax");
  } while (0)
]])], [
  ac_have_as_lzcnt=yes
  AC_MSG_RESULT([yes])
], [
  ac_have_as_lzcnt=no
  AC_MSG_RESULT([no])
])

AM_CONDITIONAL([BUILD_LZCNT_TESTS], [test x$ac_have_as_lzcnt = xyes])


# does the x86/amd64 assembler understand the LOOPNEL instruction?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_LOOPNEL_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler supports 'loopnel'])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { 		 
      __asm__ __volatile__("1:  loopnel 1b\n");
  } while (0)
]])], [
  ac_have_as_loopnel=yes
  AC_MSG_RESULT([yes])
], [
  ac_have_as_loopnel=no
  AC_MSG_RESULT([no])
])

AM_CONDITIONAL([BUILD_LOOPNEL_TESTS], [test x$ac_have_as_loopnel = xyes])


# does the x86/amd64 assembler understand ADDR32 ?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_ADDR32_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler supports 'addr32'])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { 		 
      asm volatile ("addr32 rep movsb");
  } while (0)
]])], [
  ac_have_as_addr32=yes
  AC_MSG_RESULT([yes])
], [
  ac_have_as_addr32=no
  AC_MSG_RESULT([no])
])

AM_CONDITIONAL([BUILD_ADDR32_TESTS], [test x$ac_have_as_addr32 = xyes])


# does the x86/amd64 assembler understand SSE 4.2 instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_SSE42_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks SSE4.2])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { long long int x; 
   __asm__ __volatile__(
      "crc32q %%r15,%%r15" : : : "r15" );
   __asm__ __volatile__(
      "pblendvb (%%rcx), %%xmm11" : : : "memory", "xmm11"); 
   __asm__ __volatile__(
      "aesdec %%xmm2, %%xmm1" : : : "xmm2", "xmm1"); }
  while (0)
]])], [
ac_have_as_sse42=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_sse42=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_SSE42_TESTS, test x$ac_have_as_sse42 = xyes)


# does the x86/amd64 assembler understand AVX instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_AVX_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks AVX])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { long long int x; 
   __asm__ __volatile__(
      "vmovupd (%%rsp), %%ymm7" : : : "xmm7" );
   __asm__ __volatile__(
      "vaddpd %%ymm6,%%ymm7,%%ymm8" : : : "xmm6","xmm7","xmm8"); }
  while (0)
]])], [
ac_have_as_avx=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_avx=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_AVX_TESTS, test x$ac_have_as_avx = xyes)


# does the x86/amd64 assembler understand AVX2 instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_AVX2_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks AVX2])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { long long int x; 
   __asm__ __volatile__(
      "vpsravd (%%rsp), %%ymm8, %%ymm7" : : : "xmm7", "xmm8" );
   __asm__ __volatile__(
      "vpaddb %%ymm6,%%ymm7,%%ymm8" : : : "xmm6","xmm7","xmm8"); }
  while (0)
]])], [
ac_have_as_avx2=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_avx2=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_AVX2_TESTS, test x$ac_have_as_avx2 = xyes)


# does the x86/amd64 assembler understand TSX instructions and
# the XACQUIRE/XRELEASE prefixes?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_TSX_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks TSX])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
   __asm__ __volatile__(
      "       xbegin Lfoo  \n\t"
      "Lfoo:  xend         \n\t"
      "       xacquire lock incq 0(%rsp)     \n\t"
      "       xrelease lock incq 0(%rsp)     \n"
   );
  } while (0)
]])], [
ac_have_as_tsx=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_tsx=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_TSX_TESTS, test x$ac_have_as_tsx = xyes)


# does the x86/amd64 assembler understand BMI1 and BMI2 instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_BMI_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks BMI1 and BMI2])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { unsigned int h, l;
   __asm__ __volatile__( "mulx %rax,%rcx,%r8" );
   __asm__ __volatile__(
      "andn %2, %1, %0" : "=r" (h) : "r" (0x1234567), "r" (0x7654321) );
   __asm__ __volatile__(
      "movl %2, %%edx; mulx %3, %1, %0" : "=r" (h), "=r" (l) : "g" (0x1234567), "rm" (0x7654321) : "edx" ); }
  while (0)
]])], [
ac_have_as_bmi=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_bmi=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_BMI_TESTS, test x$ac_have_as_bmi = xyes)


# does the x86/amd64 assembler understand FMA instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_FMA_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler speaks FMA])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { unsigned int h, l;
   __asm__ __volatile__(
      "vfmadd132ps (%%rsp), %%ymm8, %%ymm7" : : : "xmm7", "xmm8" );
   __asm__ __volatile__(
      "vfnmsub231sd (%%rsp), %%xmm8, %%xmm7" : : : "xmm7", "xmm8" );
   __asm__ __volatile__(
      "vfmsubadd213pd (%%rsp), %%xmm8, %%xmm7" : : : "xmm7", "xmm8" ); }
  while (0)
]])], [
ac_have_as_fma=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_fma=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_FMA_TESTS, test x$ac_have_as_fma = xyes)


# does the amd64 assembler understand MPX instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_MPX_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if amd64 assembler knows the MPX instructions])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
    asm ("bndmov %bnd0,(%rsp)");
    asm ("bndldx 3(%rbx,%rdx), %bnd2");
    asm ("bnd call foo\n"
         "bnd jmp  end\n"
         "foo: bnd ret\n"
         "end: nop");
  } while (0)
]])], [
ac_have_as_mpx=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_mpx=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_MPX_TESTS, test x$ac_have_as_mpx = xyes)


# does the amd64 assembler understand ADX instructions?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_ADX_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if amd64 assembler knows the ADX instructions])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
    asm ("adcxq %r14,%r8");
  } while (0)
]])], [
ac_have_as_adx=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_adx=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_ADX_TESTS, test x$ac_have_as_adx = xyes)


# does the amd64 assembler understand the RDRAND instruction?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_RDRAND_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if amd64 assembler knows the RDRAND instruction])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
    asm ("rdrand %r14");
    asm ("rdrand %r14d");
    asm ("rdrand %r14w");
  } while (0)
]])], [
ac_have_as_rdrand=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_rdrand=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_RDRAND_TESTS, test x$ac_have_as_rdrand = xyes)

# does the amd64 assembler understand the RDSEED instruction?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_RDSEED_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if amd64 assembler knows the RDSEED instruction])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
    asm ("rdseed %r14");
    asm ("rdseed %r14d");
    asm ("rdseed %r14w");
  } while (0)
]])], [
ac_have_as_rdseed=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_rdseed=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_RDSEED_TESTS, test x$ac_have_as_rdseed = xyes)

# does the amd64 assembler understand the F16C instructions (VCVTPH2PS and
# VCVTPS2PH) ?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_F16C_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if amd64 assembler knows the F16C instructions])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do {
    asm ("vcvtph2ps %xmm5, %ymm10");
    // If we put the dollar sign and zero together, the shell processing
    // this configure.ac script substitutes the command name in.  Sigh.
    asm ("vcvtps2ph $" "0, %ymm10, %xmm5");
  } while (0)
]])], [
ac_have_as_f16c=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_f16c=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_F16C_TESTS, test x$ac_have_as_f16c = xyes)


# does the x86/amd64 assembler understand MOVBE?
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_MOVBE_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if x86/amd64 assembler knows the MOVBE insn])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  do { long long int x; 
   __asm__ __volatile__(
      "movbe (%%rsp), %%r15" : : : "memory", "r15" ); }
  while (0)
]])], [
ac_have_as_movbe=yes
AC_MSG_RESULT([yes])
], [
ac_have_as_movbe=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_MOVBE_TESTS, test x$ac_have_as_movbe = xyes)


# Does the C compiler support the "ifunc" attribute
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_IFUNC_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if gcc supports the ifunc attribute])

AC_LINK_IFELSE([AC_LANG_SOURCE([[
static void mytest(void) {}

static void (*resolve_test(void))(void)
{
    return (void (*)(void))&mytest;
}

void test(void) __attribute__((ifunc("resolve_test")));

int main()
{
    test();
    return 0;
}
]])], [
ac_have_ifunc_attr=yes
AC_MSG_RESULT([yes])
], [
ac_have_ifunc_attr=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(BUILD_IFUNC_TESTS, test x$ac_have_ifunc_attr = xyes)

# Does the C compiler support the armv8 crc feature flag
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_ARMV8_CRC_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if gcc supports the armv8 crc feature flag])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -march=armv8-a+crc -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int main()
{
    return 0;
}
]])], [
ac_have_armv8_crc_feature=yes
AC_MSG_RESULT([yes])
], [
ac_have_armv8_crc_feature=no
AC_MSG_RESULT([no])
])
CFLAGS="$save_CFLAGS"

AM_CONDITIONAL(BUILD_ARMV8_CRC_TESTS, test x$ac_have_armv8_crc_feature = xyes)


# Does the C compiler support the armv81 flag and the assembler v8.1 instructions
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_ARMV81_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if gcc supports the armv81 feature flag and assembler supports v8.1 instructions])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -march=armv8.1-a -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int main()
{
    __asm__ __volatile__("ldadd x0, x1, [x2]" ::: "memory");
    return 0;
}
]])], [
ac_have_armv81_feature=yes
AC_MSG_RESULT([yes])
], [
ac_have_armv81_feature=no
AC_MSG_RESULT([no])
])
CFLAGS="$save_CFLAGS"

AM_CONDITIONAL(BUILD_ARMV81_TESTS, test x$ac_have_armv81_feature = xyes)


# Does the C compiler support the armv82 flag and the assembler v8.2 instructions
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_ARMV82_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if gcc supports the armv82 feature flag and assembler supports v8.2 instructions])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -march=armv8.2-a+fp16 -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int main()
{
    __asm__ __volatile__("faddp h0, v1.2h");
    return 0;
}
]])], [
ac_have_armv82_feature=yes
AC_MSG_RESULT([yes])
], [
ac_have_armv82_feature=no
AC_MSG_RESULT([no])
])
CFLAGS="$save_CFLAGS"

AM_CONDITIONAL(BUILD_ARMV82_TESTS, test x$ac_have_armv82_feature = xyes)


# Does the C compiler support the armv82-a+dotprod flag and assembler dotprod instructions
# Note, this doesn't generate a C-level symbol.  It generates a
# automake-level symbol (BUILD_ARMV82_DOTPROD_TESTS), used in test Makefile.am's
AC_MSG_CHECKING([if gcc supports the armv82-a+dotprod feature flag and assembler supports dotprod instructions])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -march=armv8.2-a+dotprod -Werror"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int main()
{
    __asm__ __volatile__("sdot v1.4s, v2.16b, v3.16b");
    return 0;
}
]])], [
ac_have_armv82_dotprod_feature=yes
AC_MSG_RESULT([yes])
], [
ac_have_armv82_dotprod_feature=no
AC_MSG_RESULT([no])
])
CFLAGS="$save_CFLAGS"

AM_CONDITIONAL(BUILD_ARMV82_DOTPROD_TESTS, test x$ac_have_armv82_dotprod_feature = xyes)


# XXX JRS 2010 Oct 13: what is this for?  For sure, we don't need this
# when building the tool executables.  I think we should get rid of it.
#
# Check for TLS support in the compiler and linker
AC_LINK_IFELSE([AC_LANG_PROGRAM([[static __thread int foo;]],
                                [[return foo;]])],
                               [vg_cv_linktime_tls=yes],
                               [vg_cv_linktime_tls=no])
# Native compilation: check whether running a program using TLS succeeds.
# Linking only is not sufficient -- e.g. on Red Hat 7.3 linking TLS programs
# succeeds but running programs using TLS fails.
# Cross-compiling: check whether linking a program using TLS succeeds.
AC_CACHE_CHECK([for TLS support], vg_cv_tls,
	       [AC_ARG_ENABLE(tls, [  --enable-tls            platform supports TLS],
		[vg_cv_tls=$enableval],
               	[AC_RUN_IFELSE([AC_LANG_PROGRAM([[static __thread int foo;]],
                                                [[return foo;]])],
                               [vg_cv_tls=yes],
                               [vg_cv_tls=no],
                               [vg_cv_tls=$vg_cv_linktime_tls])])])

if test "$vg_cv_tls" = yes -a $is_clang != applellvm; then
AC_DEFINE([HAVE_TLS], 1, [can use __thread to define thread-local variables])
fi


#----------------------------------------------------------------------------
# Solaris-specific checks.
#----------------------------------------------------------------------------

if test "$VGCONF_OS" = "solaris" ; then
AC_CHECK_HEADERS([sys/lgrp_user_impl.h])

# Solaris-specific check determining if the Sun Studio Assembler is used to
# build Valgrind.  The test checks if the x86/amd64 assembler understands the
# cmovl.l instruction, if yes then it's Sun Assembler.
#
# C-level symbol: none
# Automake-level symbol: SOLARIS_SUN_STUDIO_AS
#
AC_MSG_CHECKING([if x86/amd64 assembler speaks cmovl.l (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
]], [[
  __asm__ __volatile__("cmovl.l %edx, %eax");
]])], [
solaris_have_sun_studio_as=yes
AC_MSG_RESULT([yes])
], [
solaris_have_sun_studio_as=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_SUN_STUDIO_AS, test x$solaris_have_sun_studio_as = xyes)

# Solaris-specific check determining if symbols __xpg4 and __xpg6
# are present in linked shared libraries when gcc is invoked with -std=gnu99.
# See solaris/vgpreload-solaris.mapfile for details.
# gcc on older Solaris instructs linker to include these symbols,
# gcc on illumos and newer Solaris does not.
#
# C-level symbol: none
# Automake-level symbol: SOLARIS_XPG_SYMBOLS_PRESENT
#
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -std=gnu99"
AC_MSG_CHECKING([if xpg symbols are present with -std=gnu99 (Solaris-specific)])
temp_dir=$( /usr/bin/mktemp -d )
cat <<_ACEOF >${temp_dir}/mylib.c
#include <stdio.h>
int myfunc(void) { printf("LaPutyka\n"); }
_ACEOF
${CC} ${CFLAGS} -fpic -shared -o ${temp_dir}/mylib.so ${temp_dir}/mylib.c
xpg_present=$( /usr/bin/nm ${temp_dir}/mylib.so | ${EGREP} '(__xpg4|__xpg6)' )
if test "x${xpg_present}" = "x" ; then
    solaris_xpg_symbols_present=no
    AC_MSG_RESULT([no])
else
    solaris_xpg_symbols_present=yes
    AC_MSG_RESULT([yes])
fi
rm -rf ${temp_dir}
AM_CONDITIONAL(SOLARIS_XPG_SYMBOLS_PRESENT, test x$solaris_xpg_symbols_present = xyes)
CFLAGS="$save_CFLAGS"


# Solaris-specific check determining if gcc enables largefile support by
# default for 32-bit executables. If it does, then set SOLARIS_UNDEF_LARGESOURCE
# variable with gcc flags which disable it.
#
AC_MSG_CHECKING([if gcc enables largefile support for 32-bit apps (Solaris-specific)])
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -m32"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
  return _LARGEFILE_SOURCE;
]])], [
SOLARIS_UNDEF_LARGESOURCE="-U_LARGEFILE_SOURCE -U_LARGEFILE64_SOURCE -U_FILE_OFFSET_BITS"
AC_MSG_RESULT([yes])
], [
SOLARIS_UNDEF_LARGESOURCE=""
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS
AC_SUBST(SOLARIS_UNDEF_LARGESOURCE)


# Solaris-specific check determining if /proc/self/cmdline
# or /proc/<pid>/cmdline is supported.
#
# C-level symbol: SOLARIS_PROC_CMDLINE
# Automake-level symbol: SOLARIS_PROC_CMDLINE
#
AC_CHECK_FILE([/proc/self/cmdline],
[
solaris_proc_cmdline=yes
AC_DEFINE([SOLARIS_PROC_CMDLINE], 1,
          [Define to 1 if you have /proc/self/cmdline.])
], [
solaris_proc_cmdline=no
])
AM_CONDITIONAL(SOLARIS_PROC_CMDLINE, test x$solaris_proc_cmdline = xyes)


# Solaris-specific check determining default platform for the Valgrind launcher.
# Used in case the launcher cannot select platform by looking at the client
# image (for example because the executable is a shell script).
#
# C-level symbol: SOLARIS_LAUNCHER_DEFAULT_PLATFORM
# Automake-level symbol: none
#
AC_MSG_CHECKING([for default platform of Valgrind launcher (Solaris-specific)])
# Get the ELF class of /bin/sh first.
if ! test -f /bin/sh; then
  AC_MSG_ERROR([Shell interpreter `/bin/sh' not found.])
fi
elf_class=$( /usr/bin/file /bin/sh | sed -n 's/.*ELF \(..\)-bit.*/\1/p' )
case "$elf_class" in
  64)
    default_arch="$VGCONF_ARCH_PRI";
    ;;
  32)
    if test "x$VGCONF_ARCH_SEC" != "x"; then
      default_arch="$VGCONF_ARCH_SEC"
    else
      default_arch="$VGCONF_ARCH_PRI";
    fi
    ;;
  *)
    AC_MSG_ERROR([Cannot determine ELF class of `/bin/sh'.])
    ;;
esac
default_platform="$default_arch-$VGCONF_OS"
AC_MSG_RESULT([$default_platform])
AC_DEFINE_UNQUOTED([SOLARIS_LAUNCHER_DEFAULT_PLATFORM], ["$default_platform"],
                   [Default platform for Valgrind launcher.])


# Solaris-specific check determining if the old syscalls are available.
#
# C-level symbol: SOLARIS_OLD_SYSCALLS
# Automake-level symbol: SOLARIS_OLD_SYSCALLS
#
AC_MSG_CHECKING([for the old Solaris syscalls (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_open;
]])], [
solaris_old_syscalls=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_OLD_SYSCALLS], 1,
          [Define to 1 if you have the old Solaris syscalls.])
], [
solaris_old_syscalls=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_OLD_SYSCALLS, test x$solaris_old_syscalls = xyes)


# Solaris-specific check determining if the new accept() syscall is available.
#
# Old syscall:
# int accept(int sock, struct sockaddr *name, socklen_t *namelenp,
#            int version);
#
# New syscall (available on illumos):
# int accept(int sock, struct sockaddr *name, socklen_t *namelenp,
#            int version, int flags);
#
# If the old syscall is present then the following syscall will fail with
# ENOTSOCK (because file descriptor 0 is not a socket), if the new syscall is
# available then it will fail with EINVAL (because the flags parameter is
# invalid).
#
# C-level symbol: SOLARIS_NEW_ACCEPT_SYSCALL
# Automake-level symbol: none
#
AC_MSG_CHECKING([for the new `accept' syscall (Solaris-specific)])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
#include <errno.h>
]], [[
  errno = 0;
  syscall(SYS_accept, 0, 0, 0, 0, -1);
  return !(errno == EINVAL);
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_NEW_ACCEPT_SYSCALL], 1,
          [Define to 1 if you have the new `accept' syscall.])
], [
AC_MSG_RESULT([no])
])


# Solaris-specific check determining if the new illumos pipe() syscall is
# available.
#
# Old syscall:
# longlong_t pipe();
#
# New syscall (available on illumos):
# int pipe(intptr_t arg, int flags);
#
# If the old syscall is present then the following call will succeed, if the
# new syscall is available then it will fail with EFAULT (because address 0
# cannot be accessed).
#
# C-level symbol: SOLARIS_NEW_PIPE_SYSCALL
# Automake-level symbol: none
#
AC_MSG_CHECKING([for the new `pipe' syscall (Solaris-specific)])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
#include <errno.h>
]], [[
  errno = 0;
  syscall(SYS_pipe, 0, 0);
  return !(errno == EFAULT);
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_NEW_PIPE_SYSCALL], 1,
          [Define to 1 if you have the new `pipe' syscall.])
], [
AC_MSG_RESULT([no])
])


# Solaris-specific check determining if the new lwp_sigqueue() syscall is
# available.
#
# Old syscall:
# int lwp_kill(id_t lwpid, int sig);
#
# New syscall (available on Solaris 11):
# int lwp_sigqueue(id_t lwpid, int sig, void *value,
#                  int si_code, timespec_t *timeout);
#
# C-level symbol: SOLARIS_LWP_SIGQUEUE_SYSCALL
# Automake-level symbol: SOLARIS_LWP_SIGQUEUE_SYSCALL
#
AC_MSG_CHECKING([for the new `lwp_sigqueue' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h> 
]], [[
  return !SYS_lwp_sigqueue;
]])], [
solaris_lwp_sigqueue_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_LWP_SIGQUEUE_SYSCALL], 1,
          [Define to 1 if you have the new `lwp_sigqueue' syscall.])
], [
solaris_lwp_sigqueue_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_LWP_SIGQUEUE_SYSCALL, test x$solaris_lwp_sigqueue_syscall = xyes)


# Solaris-specific check determining if the lwp_sigqueue() syscall
# takes both pid and thread id arguments or just thread id.
#
# Old syscall (available up to Solaris 11.3):
# int lwp_sigqueue(id_t lwpid, int sig, void *value,
#                  int si_code, timespec_t *timeout);
#
# New syscall (available since Solaris 11.4):
# int lwp_sigqueue(pid_t pid, id_t lwpid, int sig, void *value,
#                  int si_code, timespec_t *timeout);
#
# If the old syscall is present then the following syscall will fail with
# EINVAL (because signal is out of range); if the new syscall is available
# then it will fail with ESRCH (because it would not find such thread in the
# current process).
#
# C-level symbol: SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID
# Automake-level symbol: SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID
#
AM_COND_IF(SOLARIS_LWP_SIGQUEUE_SYSCALL,
AC_MSG_CHECKING([if the `lwp_sigqueue' syscall accepts pid (Solaris-specific)])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
#include <errno.h>
]], [[
  errno = 0;
  syscall(SYS_lwp_sigqueue, 0, 101, 0, 0, 0, 0);
  return !(errno == ESRCH);
]])], [
solaris_lwp_sigqueue_syscall_takes_pid=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID], 1,
          [Define to 1 if you have the new `lwp_sigqueue' syscall which accepts pid.])
], [
solaris_lwp_sigqueue_syscall_takes_pid=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID,
               test x$solaris_lwp_sigqueue_syscall_takes_pid = xyes)
,
AM_CONDITIONAL(SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID, test x = y)
)


# Solaris-specific check determining if the new lwp_name() syscall is
# available.
#
# New syscall (available on Solaris 11):
# int lwp_name(int opcode, id_t lwpid, char *name, size_t len);
#
# C-level symbol: SOLARIS_LWP_NAME_SYSCALL
# Automake-level symbol: SOLARIS_LWP_NAME_SYSCALL
#
AC_MSG_CHECKING([for the new `lwp_name' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_lwp_name;
]])], [
solaris_lwp_name_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_LWP_NAME_SYSCALL], 1,
          [Define to 1 if you have the new `lwp_name' syscall.])
], [
solaris_lwp_name_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_LWP_NAME_SYSCALL, test x$solaris_lwp_name_syscall = xyes)


# Solaris-specific check determining if the new getrandom() syscall is
# available.
#
# New syscall (available on Solaris 11):
# int getrandom(void *buf, size_t buflen, uint_t flags);
#
# C-level symbol: SOLARIS_GETRANDOM_SYSCALL
# Automake-level symbol: SOLARIS_GETRANDOM_SYSCALL
#
AC_MSG_CHECKING([for the new `getrandom' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_getrandom;
]])], [
solaris_getrandom_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_GETRANDOM_SYSCALL], 1,
          [Define to 1 if you have the new `getrandom' syscall.])
], [
solaris_getrandom_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_GETRANDOM_SYSCALL, test x$solaris_getrandom_syscall = xyes)


# Solaris-specific check determining if the new zone() syscall subcodes
# ZONE_LIST_DEFUNCT and ZONE_GETATTR_DEFUNCT are available.  These subcodes
# were added in Solaris 11 but are missing on illumos.
#
# C-level symbol: SOLARIS_ZONE_DEFUNCT
# Automake-level symbol: SOLARIS_ZONE_DEFUNCT
#
AC_MSG_CHECKING([for ZONE_LIST_DEFUNCT and ZONE_GETATTR_DEFUNCT (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/zone.h>
]], [[
  return !(ZONE_LIST_DEFUNCT && ZONE_GETATTR_DEFUNCT);
]])], [
solaris_zone_defunct=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_ZONE_DEFUNCT], 1,
          [Define to 1 if you have the `ZONE_LIST_DEFUNCT' and `ZONE_GETATTR_DEFUNC' constants.])
], [
solaris_zone_defunct=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_ZONE_DEFUNCT, test x$solaris_zone_defunct = xyes)


# Solaris-specific check determining if commands A_GETSTAT and A_SETSTAT
# for auditon(2) subcode of the auditsys() syscall are available.
# These commands are available in Solaris 11 and illumos but were removed
# in Solaris 11.4.
#
# C-level symbol: SOLARIS_AUDITON_STAT
# Automake-level symbol: SOLARIS_AUDITON_STAT
#
AC_MSG_CHECKING([for A_GETSTAT and A_SETSTAT auditon(2) commands (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <bsm/audit.h>
]], [[
  return !(A_GETSTAT && A_SETSTAT);
]])], [
solaris_auditon_stat=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_AUDITON_STAT], 1,
          [Define to 1 if you have the `A_GETSTAT' and `A_SETSTAT' constants.])
], [
solaris_auditon_stat=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_AUDITON_STAT, test x$solaris_auditon_stat = xyes)


# Solaris-specific check determining if the new shmsys() syscall subcodes
# IPC_XSTAT64, SHMADV, SHM_ADV_GET, SHM_ADV_SET and SHMGET_OSM are available.
# These subcodes were added in Solaris 11 but are missing on illumos.
#
# C-level symbol: SOLARIS_SHM_NEW
# Automake-level symbol: SOLARIS_SHM_NEW
#
AC_MSG_CHECKING([for SHMADV, SHM_ADV_GET, SHM_ADV_SET and SHMGET_OSM (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/ipc_impl.h>
#include <sys/shm.h>
#include <sys/shm_impl.h>
]], [[
  return !(IPC_XSTAT64 && SHMADV && SHM_ADV_GET && SHM_ADV_SET && SHMGET_OSM);
]])], [
solaris_shm_new=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_SHM_NEW], 1,
          [Define to 1 if you have the `IPC_XSTAT64', `SHMADV', `SHM_ADV_GET', `SHM_ADV_SET' and `SHMGET_OSM' constants.])
], [
solaris_shm_new=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_SHM_NEW, test x$solaris_shm_new = xyes)


# Solaris-specific check determining if prxregset_t is available.  Illumos
# currently does not define it on the x86 platform.
#
# C-level symbol: SOLARIS_PRXREGSET_T
# Automake-level symbol: SOLARIS_PRXREGSET_T
#
AC_MSG_CHECKING([for the `prxregset_t' type (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/procfs_isa.h>
]], [[
  return !sizeof(prxregset_t);
]])], [
solaris_prxregset_t=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_PRXREGSET_T], 1,
          [Define to 1 if you have the `prxregset_t' type.])
], [
solaris_prxregset_t=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_PRXREGSET_T, test x$solaris_prxregset_t = xyes)


# Solaris-specific check determining if the new frealpathat() syscall is
# available.
#
# New syscall (available on Solaris 11.1):
# int frealpathat(int fd, char *path, char *buf, size_t buflen);
#
# C-level symbol: SOLARIS_FREALPATHAT_SYSCALL
# Automake-level symbol: SOLARIS_FREALPATHAT_SYSCALL
#
AC_MSG_CHECKING([for the new `frealpathat' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_frealpathat;
]])], [
solaris_frealpathat_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_FREALPATHAT_SYSCALL], 1,
          [Define to 1 if you have the new `frealpathat' syscall.])
], [
solaris_frealpathat_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_FREALPATHAT_SYSCALL, test x$solaris_frealpathat_syscall = xyes)


# Solaris-specific check determining if the new uuidsys() syscall is
# available.
#
# New syscall (available on newer Solaris):
# int uuidsys(struct uuid *uuid);
#
# C-level symbol: SOLARIS_UUIDSYS_SYSCALL
# Automake-level symbol: SOLARIS_UUIDSYS_SYSCALL
#
AC_MSG_CHECKING([for the new `uuidsys' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_uuidsys;
]])], [
solaris_uuidsys_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_UUIDSYS_SYSCALL], 1,
          [Define to 1 if you have the new `uuidsys' syscall.])
], [
solaris_uuidsys_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_UUIDSYS_SYSCALL, test x$solaris_uuidsys_syscall = xyes)


# Solaris-specific check determining if the new labelsys() syscall subcode
# TNDB_GET_TNIP is available.  This subcode was added in Solaris 11 but is
# missing on illumos.
#
# C-level symbol: SOLARIS_TNDB_GET_TNIP
# Automake-level symbol: SOLARIS_TNDB_GET_TNIP
#
AC_MSG_CHECKING([for TNDB_GET_TNIP (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/tsol/tndb.h>
]], [[
  return !TNDB_GET_TNIP;
]])], [
solaris_tndb_get_tnip=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_TNDB_GET_TNIP], 1,
          [Define to 1 if you have the `TNDB_GET_TNIP' constant.])
], [
solaris_tndb_get_tnip=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_TNDB_GET_TNIP, test x$solaris_tndb_get_tnip = xyes)


# Solaris-specific check determining if the new labelsys() syscall opcodes
# TSOL_GETCLEARANCE and TSOL_SETCLEARANCE are available. These opcodes were
# added in Solaris 11 but are missing on illumos.
#
# C-level symbol: SOLARIS_TSOL_CLEARANCE
# Automake-level symbol: SOLARIS_TSOL_CLEARANCE
#
AC_MSG_CHECKING([for TSOL_GETCLEARANCE and TSOL_SETCLEARANCE (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/tsol/tsyscall.h>
]], [[
  return !(TSOL_GETCLEARANCE && TSOL_SETCLEARANCE);
]])], [
solaris_tsol_clearance=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_TSOL_CLEARANCE], 1,
          [Define to 1 if you have the `TSOL_GETCLEARANCE' and `TSOL_SETCLEARANCE' constants.])
], [
solaris_tsol_clearance=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_TSOL_CLEARANCE, test x$solaris_tsol_clearance = xyes)


# Solaris-specific check determining if the new pset() syscall subcode
# PSET_GET_NAME is available. This subcode was added in Solaris 11.4 but
# is missing on illumos and Solaris 11.3.
#
# C-level symbol: SOLARIS_PSET_GET_NAME
# Automake-level symbol: SOLARIS_PSET_GET_NAME
#
AC_MSG_CHECKING([for PSET_GET_NAME (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/pset.h>
]], [[
  return !(PSET_GET_NAME);
]])], [
solaris_pset_get_name=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_PSET_GET_NAME], 1,
          [Define to 1 if you have the `PSET_GET_NAME' constants.])
], [
solaris_pset_get_name=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_PSET_GET_NAME, test x$solaris_pset_get_name = xyes)


# Solaris-specific check determining if the utimesys() syscall is
# available (on illumos and older Solaris).
#
# C-level symbol: SOLARIS_UTIMESYS_SYSCALL
# Automake-level symbol: SOLARIS_UTIMESYS_SYSCALL
#
AC_MSG_CHECKING([for the `utimesys' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_utimesys;
]])], [
solaris_utimesys_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_UTIMESYS_SYSCALL], 1,
          [Define to 1 if you have the `utimesys' syscall.])
], [
solaris_utimesys_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_UTIMESYS_SYSCALL, test x$solaris_utimesys_syscall = xyes)


# Solaris-specific check determining if the utimensat() syscall is
# available (on newer Solaris).
#
# C-level symbol: SOLARIS_UTIMENSAT_SYSCALL
# Automake-level symbol: SOLARIS_UTIMENSAT_SYSCALL
#
AC_MSG_CHECKING([for the `utimensat' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_utimensat;
]])], [
solaris_utimensat_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_UTIMENSAT_SYSCALL], 1,
          [Define to 1 if you have the `utimensat' syscall.])
], [
solaris_utimensat_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_UTIMENSAT_SYSCALL, test x$solaris_utimensat_syscall = xyes)


# Solaris-specific check determining if the spawn() syscall is available
# (on newer Solaris).
#
# C-level symbol: SOLARIS_SPAWN_SYSCALL
# Automake-level symbol: SOLARIS_SPAWN_SYSCALL
#
AC_MSG_CHECKING([for the `spawn' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_spawn;
]])], [
solaris_spawn_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_SPAWN_SYSCALL], 1,
          [Define to 1 if you have the `spawn' syscall.])
], [
solaris_spawn_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_SPAWN_SYSCALL, test x$solaris_spawn_syscall = xyes)


# Solaris-specific check determining if commands MODNVL_CTRLMAP through
# MODDEVINFO_CACHE_TS for modctl() syscall are available (on newer Solaris).
#
# C-level symbol: SOLARIS_MODCTL_MODNVL
# Automake-level symbol: SOLARIS_MODCTL_MODNVL
#
AC_MSG_CHECKING([for MODNVL_CTRLMAP through MODDEVINFO_CACHE_TS modctl(2) commands (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/modctl.h>
]], [[
  return !(MODNVL_CTRLMAP && MODDEVINFO_CACHE_TS);
]])], [
solaris_modctl_modnvl=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_MODCTL_MODNVL], 1,
          [Define to 1 if you have the `MODNVL_CTRLMAP' through `MODDEVINFO_CACHE_TS' constants.])
], [
solaris_modctl_modnvl=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_MODCTL_MODNVL, test x$solaris_modctl_modnvl = xyes)


# Solaris-specific check determining whether nscd (name switch cache daemon)
# attaches its door at /system/volatile/name_service_door (Solaris)
# or at /var/run/name_service_door (illumos).
#
# Note that /var/run is a symlink to /system/volatile on Solaris
# but not vice versa on illumos.
#
# C-level symbol: SOLARIS_NSCD_DOOR_SYSTEM_VOLATILE
# Automake-level symbol: SOLARIS_NSCD_DOOR_SYSTEM_VOLATILE
#
AC_MSG_CHECKING([for nscd door location (Solaris-specific)])
if test -e /system/volatile/name_service_door; then
    solaris_nscd_door_system_volatile=yes
    AC_MSG_RESULT([/system/volatile/name_service_door])
    AC_DEFINE([SOLARIS_NSCD_DOOR_SYSTEM_VOLATILE], 1,
              [Define to 1 if nscd attaches to /system/volatile/name_service_door.])
else
    solaris_nscd_door_system_volatile=no
    AC_MSG_RESULT([/var/run/name_service_door])
fi
AM_CONDITIONAL(SOLARIS_NSCD_DOOR_SYSTEM_VOLATILE, test x$solaris_nscd_door_system_volatile = xyes)


# Solaris-specific check determining if the new gethrt() fasttrap is available.
#
# New fasttrap (available on Solaris 11):
# hrt_t *gethrt(void);
#
# C-level symbol: SOLARIS_GETHRT_FASTTRAP
# Automake-level symbol: SOLARIS_GETHRT_FASTTRAP
#
AC_MSG_CHECKING([for the new `gethrt' fasttrap (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/trap.h>
]], [[
  return !T_GETHRT;
]])], [
solaris_gethrt_fasttrap=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_GETHRT_FASTTRAP], 1,
          [Define to 1 if you have the new `gethrt' fasttrap.])
], [
solaris_gethrt_fasttrap=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_GETHRT_FASTTRAP, test x$solaris_gethrt_fasttrap = xyes)


# Solaris-specific check determining if the new get_zone_offset() fasttrap
# is available.
#
# New fasttrap (available on Solaris 11):
# zonehrtoffset_t *get_zone_offset(void);
#
# C-level symbol: SOLARIS_GETZONEOFFSET_FASTTRAP
# Automake-level symbol: SOLARIS_GETZONEOFFSET_FASTTRAP
#
AC_MSG_CHECKING([for the new `get_zone_offset' fasttrap (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/trap.h>
]], [[
  return !T_GETZONEOFFSET;
]])], [
solaris_getzoneoffset_fasttrap=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_GETZONEOFFSET_FASTTRAP], 1,
          [Define to 1 if you have the new `get_zone_offset' fasttrap.])
], [
solaris_getzoneoffset_fasttrap=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_GETZONEOFFSET_FASTTRAP, test x$solaris_getzoneoffset_fasttrap = xyes)


# Solaris-specific check determining if the execve() syscall
# takes fourth argument (flags) or not.
#
# Old syscall (available on illumos):
# int execve(const char *fname, const char **argv, const char **envp);
#
# New syscall (available on Solaris):
# int execve(uintptr_t file, const char **argv, const char **envp, int flags);
#
# If the new syscall is present then it will fail with EINVAL (because flags
# are invalid); if the old syscall is available then it will fail with ENOENT
# (because the file could not be found).
#
# C-level symbol: SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS
# Automake-level symbol: SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS
#
AC_MSG_CHECKING([if the `execve' syscall accepts flags (Solaris-specific)])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
#include <errno.h>
]], [[
  errno = 0;
  syscall(SYS_execve, "/no/existing/path", 0, 0, 0xdeadbeef, 0, 0);
  return !(errno == EINVAL);
]])], [
solaris_execve_syscall_takes_flags=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS], 1,
          [Define to 1 if you have the new `execve' syscall which accepts flags.])
], [
solaris_execve_syscall_takes_flags=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS,
               test x$solaris_execve_syscall_takes_flags = xyes)


# Solaris-specific check determining version of the repository cache protocol.
# Every Solaris version uses a different one, ranging from 21 to current 25.
# The check is very ugly, though.
#
# C-level symbol: SOLARIS_REPCACHE_PROTOCOL_VERSION vv
# Automake-level symbol: none
#
AC_PATH_PROG(DIS_PATH, dis, false)
if test "x$DIS_PATH" = "xfalse"; then
  AC_MSG_FAILURE([Object code disassembler (`dis') not found.])
fi
# The illumos source is (or was) here
# https://github.com/illumos/illumos-gate/blob/master/usr/src/lib/libscf/common/lowlevel.c#L1148
# specifically the line
#
# request.rdr_version = REPOSITORY_DOOR_VERSION;
#
# rdr_version is a 32bit unsigned int
# The macro REPOSITORY_DOOR_VERSION contains the ascii letters "Rep" in the top 3
# bytes and the door version in the lowest byte. Hence we look for Rep which is 526570
# in hex and then extrace the following byte.
AC_CHECK_LIB(scf, scf_handle_bind, [], [
  AC_MSG_WARN([Function `scf_handle_bind' was not found in `libscf'.])
  AC_MSG_ERROR([Cannot determine version of the repository cache protocol.])
])

AC_MSG_CHECKING([for version of the repository cache protocol (Solaris-specific)])
if test "X$VGCONF_ARCH_PRI" = "Xamd64"; then
  libscf=/usr/lib/64/libscf.so.1
else
  libscf=/usr/lib/libscf.so.1
fi
if ! $DIS_PATH -F scf_handle_bind $libscf  | grep -q -E '0x(4d01)?526570'; then
  AC_MSG_WARN([Function `scf_handle_bind' does not contain repository cache protocol version.])
  AC_MSG_ERROR([Cannot determine version of the repository cache protocol.])
fi
hex=$( $DIS_PATH -F scf_handle_bind $libscf  | grep 526570 | sed 's/.*526570//;s/,.*//' )
if test -z "$hex"; then
  AC_MSG_WARN([Version of the repository cache protocol is empty?!])
  AC_MSG_ERROR([Cannot determine version of the repository cache protocol.])
fi
version=$( printf "%d\n" 0x$hex )
AC_MSG_RESULT([$version])
AC_DEFINE_UNQUOTED([SOLARIS_REPCACHE_PROTOCOL_VERSION], [$version],
                   [Version number of the repository door cache protocol.])


# Solaris-specific check determining if "sysstat" segment reservation type
# is available.
#
# New "sysstat" segment reservation (available on Solaris 11.4):
# - program header type:    PT_SUNW_SYSSTAT
# - auxiliary vector entry: AT_SUN_SYSSTAT_ADDR
#
# C-level symbol: SOLARIS_RESERVE_SYSSTAT_ADDR
# Automake-level symbol: SOLARIS_RESERVE_SYSSTAT_ADDR
#
AC_MSG_CHECKING([for the new `sysstat' segment reservation (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/auxv.h>
]], [[
  return !AT_SUN_SYSSTAT_ADDR;
]])], [
solaris_reserve_sysstat_addr=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_RESERVE_SYSSTAT_ADDR], 1,
          [Define to 1 if you have the new `sysstat' segment reservation.])
], [
solaris_reserve_sysstat_addr=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_RESERVE_SYSSTAT_ADDR, test x$solaris_reserve_sysstat_addr = xyes)


# Solaris-specific check determining if "sysstat_zone" segment reservation type
# is available.
#
# New "sysstat_zone" segment reservation (available on Solaris 11.4):
# - program header type:    PT_SUNW_SYSSTAT_ZONE
# - auxiliary vector entry: AT_SUN_SYSSTAT_ZONE_ADDR
#
# C-level symbol: SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR
# Automake-level symbol: SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR
#
AC_MSG_CHECKING([for the new `sysstat_zone' segment reservation (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/auxv.h>
]], [[
  return !AT_SUN_SYSSTAT_ZONE_ADDR;
]])], [
solaris_reserve_sysstat_zone_addr=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR], 1,
          [Define to 1 if you have the new `sysstat_zone' segment reservation.])
], [
solaris_reserve_sysstat_zone_addr=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR, test x$solaris_reserve_sysstat_zone_addr = xyes)


# Solaris-specific check determining if the system_stats() syscall is available
# (on newer Solaris).
#
# C-level symbol: SOLARIS_SYSTEM_STATS_SYSCALL
# Automake-level symbol: SOLARIS_SYSTEM_STATS_SYSCALL
#
AC_MSG_CHECKING([for the `system_stats' syscall (Solaris-specific)])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/syscall.h>
]], [[
  return !SYS_system_stats;
]])], [
solaris_system_stats_syscall=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_SYSTEM_STATS_SYSCALL], 1,
          [Define to 1 if you have the `system_stats' syscall.])
], [
solaris_system_stats_syscall=no
AC_MSG_RESULT([no])
])
AM_CONDITIONAL(SOLARIS_SYSTEM_STATS_SYSCALL, test x$solaris_system_stats_syscall = xyes)


# Solaris-specific check determining if fpregset_t defines struct _fpchip_state
# (on newer illumos) or struct fpchip_state (Solaris, older illumos).
#
# C-level symbol: SOLARIS_FPCHIP_STATE_TAKES_UNDERSCORE
# Automake-level symbol: none
#
AC_CHECK_TYPE([struct _fpchip_state],
              [solaris_fpchip_state_takes_underscore=yes],
              [solaris_fpchip_state_takes_underscore=no],
              [[#include <sys/regset.h>]])
if test "$solaris_fpchip_state_takes_underscore" = "yes"; then
  AC_DEFINE(SOLARIS_FPCHIP_STATE_TAKES_UNDERSCORE, 1,
            [Define to 1 if fpregset_t defines struct _fpchip_state])
fi


# Solaris-specific check determining if schedctl page shared between kernel
# and userspace program is executable (illumos, older Solaris) or not (newer
# Solaris).
#
# C-level symbol: SOLARIS_SCHEDCTL_PAGE_EXEC
# Automake-level symbol: none
#
AC_MSG_CHECKING([if schedctl page is executable (Solaris-specific)])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <assert.h>
#include <fcntl.h>
#include <procfs.h>
#include <schedctl.h>
#include <stdio.h>
#include <unistd.h>
]], [[
    schedctl_t *scp = schedctl_init();
    if (scp == NULL)
        return 1;

    int fd = open("/proc/self/map", O_RDONLY);
    assert(fd >= 0);

    prmap_t map;
    ssize_t rd;
    while ((rd = read(fd, &map, sizeof(map))) == sizeof(map)) {
        if (map.pr_vaddr == ((uintptr_t) scp & PAGEMASK)) {
            fprintf(stderr, "%#lx [%zu] %s\n", map.pr_vaddr, map.pr_size,
                    (map.pr_mflags & MA_EXEC) ? "x" : "no-x");
            return (map.pr_mflags & MA_EXEC);
        }
    }

    return 1;
]])], [
solaris_schedctl_page_exec=no
AC_MSG_RESULT([no])
], [
solaris_schedctl_page_exec=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_SCHEDCTL_PAGE_EXEC], 1,
          [Define to 1 if you have the schedctl page executable.])
])


# Solaris-specific check determining if PT_SUNWDTRACE program header provides
# scratch space for DTrace fasttrap provider (illumos, older Solaris) or just
# an initial thread pointer for libc (newer Solaris).
#
# C-level symbol: SOLARIS_PT_SUNDWTRACE_THRP
# Automake-level symbol: none
#
AC_MSG_CHECKING([if PT_SUNWDTRACE serves for initial thread pointer (Solaris-specific)])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <sys/fasttrap_isa.h>
]], [[
    return !FT_SCRATCHSIZE;
]])], [
solaris_pt_sunwdtrace_thrp=yes
AC_MSG_RESULT([yes])
AC_DEFINE([SOLARIS_PT_SUNDWTRACE_THRP], 1,
          [Define to 1 if PT_SUNWDTRACE program header provides just an initial thread pointer for libc.])
], [
solaris_pt_sunwdtrace_thrp=no
AC_MSG_RESULT([no])
])

else
AM_CONDITIONAL(SOLARIS_SUN_STUDIO_AS, false)
AM_CONDITIONAL(SOLARIS_XPG_SYMBOLS_PRESENT, false)
AM_CONDITIONAL(SOLARIS_PROC_CMDLINE, false)
AM_CONDITIONAL(SOLARIS_OLD_SYSCALLS, false)
AM_CONDITIONAL(SOLARIS_LWP_SIGQUEUE_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID, false)
AM_CONDITIONAL(SOLARIS_LWP_NAME_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_GETRANDOM_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_ZONE_DEFUNCT, false)
AM_CONDITIONAL(SOLARIS_AUDITON_STAT, false)
AM_CONDITIONAL(SOLARIS_SHM_NEW, false)
AM_CONDITIONAL(SOLARIS_PRXREGSET_T, false)
AM_CONDITIONAL(SOLARIS_FREALPATHAT_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_UUIDSYS_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_TNDB_GET_TNIP, false)
AM_CONDITIONAL(SOLARIS_TSOL_CLEARANCE, false)
AM_CONDITIONAL(SOLARIS_PSET_GET_NAME, false)
AM_CONDITIONAL(SOLARIS_UTIMESYS_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_UTIMENSAT_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_SPAWN_SYSCALL, false)
AM_CONDITIONAL(SOLARIS_MODCTL_MODNVL, false)
AM_CONDITIONAL(SOLARIS_NSCD_DOOR_SYSTEM_VOLATILE, false)
AM_CONDITIONAL(SOLARIS_GETHRT_FASTTRAP, false)
AM_CONDITIONAL(SOLARIS_GETZONEOFFSET_FASTTRAP, false)
AM_CONDITIONAL(SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS, false)
AM_CONDITIONAL(SOLARIS_RESERVE_SYSSTAT_ADDR, false)
AM_CONDITIONAL(SOLARIS_RESERVE_SYSSTAT_ZONE_ADDR, false)
AM_CONDITIONAL(SOLARIS_SYSTEM_STATS_SYSCALL, false)
fi # test "$VGCONF_OS" = "solaris"

#----------------------------------------------------------------------------
# FreeBSD-specific checks.
#----------------------------------------------------------------------------

# Rather than having a large number of feature test as above with Solaris
# these tests are per-version. This may not be entirely reliable for
# FreeBSD development branches (XX.Y-CURRENT) or pre-release branches
# (XX.Y-STABLE) but it should work for XX-Y-RELEASE

if test "$VGCONF_OS" = "freebsd" ; then

AM_CONDITIONAL(FREEBSD_VERS_13_PLUS, test $freebsd_vers -ge $freebsd_13_0)
AM_CONDITIONAL(FREEBSD_VERS_15_PLUS, test $freebsd_vers -ge $freebsd_15)

else

AM_CONDITIONAL(FREEBSD_VERS_13_PLUS, false)
AM_CONDITIONAL(FREEBSD_VERS_15_PLUS, false)

fi # test "$VGCONF_OS" = "freebsd"


#----------------------------------------------------------------------------
# Checks for C header files.
#----------------------------------------------------------------------------

AC_CHECK_HEADERS([       \
        asm/unistd.h     \
        endian.h         \
        mqueue.h         \
        sys/endian.h     \
        sys/epoll.h      \
        sys/eventfd.h    \
        sys/klog.h       \
        sys/poll.h       \
        sys/prctl.h      \
        sys/signal.h     \
        sys/signalfd.h   \
        sys/syscall.h    \
        sys/sysnvl.h     \
        sys/time.h       \
        sys/types.h      \
        ])

# Verify whether the <linux/futex.h> header is usable.
AC_MSG_CHECKING([if <linux/futex.h> is usable])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -D__user="
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <linux/futex.h>
]], [[
  return FUTEX_WAIT;
]])], [
ac_have_usable_linux_futex_h=yes
AC_DEFINE([HAVE_USABLE_LINUX_FUTEX_H], 1,
          [Define to 1 if you have a usable <linux/futex.h> header file.])
AC_MSG_RESULT([yes])
], [
ac_have_usable_linux_futex_h=no
AC_MSG_RESULT([no])
])
CFLAGS="$save_CFLAGS"


#----------------------------------------------------------------------------
# Checks for typedefs, structures, and compiler characteristics.
#----------------------------------------------------------------------------
AC_TYPE_UID_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_CHECK_HEADERS_ONCE([sys/time.h])

AC_CHECK_TYPE([struct statx], [
  AC_DEFINE([HAVE_STRUCT_STATX_IN_SYS_STAT_H], 1,
            [Define to 1 if <sys/stat.h> declares struct statx.])
], [], [
  #define _GNU_SOURCE
  #include <sys/stat.h>
])


#----------------------------------------------------------------------------
# Checks for library functions.
#----------------------------------------------------------------------------
AC_FUNC_MEMCMP
AC_FUNC_MMAP

AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([rt], [clock_gettime])

AC_CHECK_FUNCS([     \
        aligned_alloc \
        clock_gettime\
        copy_file_range \
        epoll_create \
        epoll_pwait  \
        getaddrinfo  \
        klogctl      \
        mallinfo     \
        memchr       \
        memfd_create \
        memset       \
        mkdir        \
        mremap       \
        pipe2	     \
        ppoll        \
        preadv       \
        preadv2      \
        process_vm_readv  \
        process_vm_writev \
        pthread_barrier_init       \
        pthread_condattr_setclock  \
        pthread_mutex_timedlock    \
        pthread_rwlock_timedrdlock \
        pthread_rwlock_timedwrlock \
        pthread_setname_np         \
        pthread_spin_lock          \
        pthread_yield              \
        pwritev      \
        pwritev2     \
        rawmemchr    \
        readlinkat   \
        reallocarray \
        semtimedop   \
        setcontext   \
        signalfd     \
        sigwaitinfo  \
        strchr       \
        strdup       \
        strpbrk      \
        strrchr      \
        strstr       \
        swapcontext  \
        syscall      \
        utimensat    \
        mempcpy      \
        strlcat      \
        strlcpy      \
        stpncpy      \
        strchrnul    \
        memrchr      \
        strndup      \
        close_range  \
        wcsncpy
        ])

# AC_CHECK_LIB adds any library found to the variable LIBS, and links these
# libraries with any shared object and/or executable. This is NOT what we
# want for e.g. vgpreload_core-x86-linux.so
LIBS=""

AM_CONDITIONAL([HAVE_PTHREAD_BARRIER],
               [test x$ac_cv_func_pthread_barrier_init = xyes])
AM_CONDITIONAL([HAVE_PTHREAD_MUTEX_TIMEDLOCK],
               [test x$ac_cv_func_pthread_mutex_timedlock = xyes])
AM_CONDITIONAL([HAVE_PTHREAD_SPINLOCK],
               [test x$ac_cv_func_pthread_spin_lock = xyes])
AM_CONDITIONAL([HAVE_PTHREAD_SETNAME_NP],
               [test x$ac_cv_func_pthread_setname_np = xyes])
AM_CONDITIONAL([HAVE_COPY_FILE_RANGE],
               [test x$ac_cv_func_copy_file_range = xyes])
AM_CONDITIONAL([HAVE_PREADV_PWRITEV],
               [test x$ac_cv_func_preadv = xyes && test x$ac_cv_func_pwritev = xyes])
AM_CONDITIONAL([HAVE_PREADV2_PWRITEV2],
               [test x$ac_cv_func_preadv2 = xyes && test x$ac_cv_func_pwritev2 = xyes])
AM_CONDITIONAL([HAVE_SETCONTEXT], [test x$ac_cv_func_setcontext = xyes])
AM_CONDITIONAL([HAVE_SWAPCONTEXT], [test x$ac_cv_func_swapcontext = xyes])
AM_CONDITIONAL([HAVE_MEMFD_CREATE],
               [test x$ac_cv_func_memfd_create = xyes])
AM_CONDITIONAL([HAVE_GETADDRINFO],
               [test x$ac_cv_func_getaddrinfo = xyes])
AM_CONDITIONAL([HAVE_CLOSE_RANGE],
               [test x$ac_cv_func_close_range = xyes])
AM_CONDITIONAL([HAVE_REALLOCARRAY],
               [test x$ac_cv_func_reallocarray = xyes])
AM_CONDITIONAL([HAVE_WCSNCPY],
               [test x$ac_cv_func_wcsncpy = xyes])
AM_CONDITIONAL([HAVE_STRLCAT],
               [test x$ac_cv_func_strlcat = xyes])
AM_CONDITIONAL([HAVE_STRLCPY],
               [test x$ac_cv_func_strlcpy = xyes])

if test x$VGCONF_PLATFORM_PRI_CAPS = xMIPS32_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xMIPS64_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xNANOMIPS_LINUX; then
  AC_DEFINE([DISABLE_PTHREAD_SPINLOCK_INTERCEPT], 1,
            [Disable intercept pthread_spin_lock() on MIPS32, MIPS64 and nanoMIPS.])
fi

#----------------------------------------------------------------------------
# MPI checks
#----------------------------------------------------------------------------
# Do we have a useable MPI setup on the primary and/or secondary targets?
# On Linux, by default, assumes mpicc and -m32/-m64
# Note: this is a kludge in that it assumes the specified mpicc 
# understands -m32/-m64 regardless of what is specified using
# --with-mpicc=.
AC_PATH_PROG([MPI_CC], [mpicc], [mpicc],
             [$PATH:/usr/lib/openmpi/bin:/usr/lib64/openmpi/bin])

mflag_primary=
if test x$VGCONF_PLATFORM_PRI_CAPS = xX86_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xX86_FREEBSD \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xPPC32_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xARM_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xMIPS32_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xNANOMIPS_LINUX \
     -o x$VGCONF_PLATFORM_PRI_CAPS = xX86_SOLARIS ; then
  mflag_primary=$FLAG_M32
elif test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_LINUX \
       -o x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_FREEBSD \
       -o x$VGCONF_PLATFORM_PRI_CAPS = xPPC64_LINUX \
       -o x$VGCONF_PLATFORM_PRI_CAPS = xARM64_LINUX \
       -o x$VGCONF_PLATFORM_PRI_CAPS = xMIPS64_LINUX \
       -o x$VGCONF_PLATFORM_PRI_CAPS = xS390X_LINUX ; then
  mflag_primary=$FLAG_M64
elif test x$VGCONF_PLATFORM_PRI_CAPS = xX86_DARWIN ; then
  mflag_primary="$FLAG_M32 -arch i386"
elif test x$VGCONF_PLATFORM_PRI_CAPS = xAMD64_DARWIN ; then
  mflag_primary="$FLAG_M64 -arch x86_64"
fi

mflag_secondary=
if test x$VGCONF_PLATFORM_SEC_CAPS = xX86_LINUX \
     -o x$VGCONF_PLATFORM_SEC_CAPS = xPPC32_LINUX \
     -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_SOLARIS \
     -o x$VGCONF_PLATFORM_SEC_CAPS = xMIPS32_LINUX \
     -o x$VGCONF_PLATFORM_SEC_CAPS = xX86_FREEBSD ; then
  mflag_secondary=$FLAG_M32
elif test x$VGCONF_PLATFORM_SEC_CAPS = xX86_DARWIN ; then
  mflag_secondary="$FLAG_M32 -arch i386"
fi


AC_ARG_WITH(mpicc,
   [  --with-mpicc=           Specify name of MPI2-ised C compiler],
   MPI_CC=$withval
)
AC_SUBST(MPI_CC)

## We AM_COND_IF here instead of automake "if" in mpi/Makefile.am so that we can
## use these values in the check for a functioning mpicc.
##
## We leave the MPI_FLAG_M3264_ logic in mpi/Makefile.am and assume that
## mflag_primary/mflag_secondary are sufficient approximations of that behavior
AM_COND_IF([VGCONF_OS_IS_LINUX],
           [CFLAGS_MPI="-g -O -fno-omit-frame-pointer -Wall -fpic"
            LDFLAGS_MPI="-fpic -shared"])
AM_COND_IF([VGCONF_OS_IS_FREEBSD],
           [CFLAGS_MPI="-g -O -fno-omit-frame-pointer -Wall -fpic"
            LDFLAGS_MPI="-fpic -shared"])
AM_COND_IF([VGCONF_OS_IS_DARWIN],
           [CFLAGS_MPI="-g -O -fno-omit-frame-pointer -Wall -dynamic"
            LDFLAGS_MPI="-dynamic -dynamiclib -all_load"])
AM_COND_IF([VGCONF_OS_IS_SOLARIS],
           [CFLAGS_MPI="-g -O -fno-omit-frame-pointer -Wall -fpic"
            LDFLAGS_MPI="-fpic -shared"])

AC_SUBST([CFLAGS_MPI])
AC_SUBST([LDFLAGS_MPI])


## See if MPI_CC works for the primary target
##
AC_MSG_CHECKING([primary target for usable MPI2-compliant C compiler and mpi.h])
saved_CC=$CC
saved_CFLAGS=$CFLAGS
CC=$MPI_CC
CFLAGS="$CFLAGS_MPI $mflag_primary"
saved_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS_MPI $mflag_primary"
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <mpi.h>
#include <stdio.h>
]], [[
  int ni, na, nd, comb;
  int r = MPI_Init(NULL,NULL);
  r |= MPI_Type_get_envelope( MPI_INT, &ni, &na, &nd, &comb );
  r |= MPI_Finalize();
  return r; 
]])], [
ac_have_mpi2_pri=yes
AC_MSG_RESULT([yes, $MPI_CC])
], [
ac_have_mpi2_pri=no
AC_MSG_RESULT([no])
])
CC=$saved_CC
CFLAGS=$saved_CFLAGS
LDFLAGS="$saved_LDFLAGS"
AM_CONDITIONAL(BUILD_MPIWRAP_PRI, test x$ac_have_mpi2_pri = xyes)

## See if MPI_CC works for the secondary target.  Complication: what if
## there is no secondary target?  We need this to then fail.
## Kludge this by making MPI_CC something which will surely fail in
## such a case.
##
AC_MSG_CHECKING([secondary target for usable MPI2-compliant C compiler and mpi.h])
saved_CC=$CC
saved_CFLAGS=$CFLAGS
saved_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS_MPI $mflag_secondary"
if test x$VGCONF_PLATFORM_SEC_CAPS = x ; then
  CC="$MPI_CC this will surely fail"
else
  CC=$MPI_CC
fi
CFLAGS="$CFLAGS_MPI $mflag_secondary"
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <mpi.h>
#include <stdio.h>
]], [[
  int ni, na, nd, comb;
  int r = MPI_Init(NULL,NULL);
  r |= MPI_Type_get_envelope( MPI_INT, &ni, &na, &nd, &comb );
  r |= MPI_Finalize();
  return r; 
]])], [
ac_have_mpi2_sec=yes
AC_MSG_RESULT([yes, $MPI_CC])
], [
ac_have_mpi2_sec=no
AC_MSG_RESULT([no])
])
CC=$saved_CC
CFLAGS=$saved_CFLAGS
LDFLAGS="$saved_LDFLAGS"
AM_CONDITIONAL(BUILD_MPIWRAP_SEC, test x$ac_have_mpi2_sec = xyes)


#----------------------------------------------------------------------------
# Other library checks
#----------------------------------------------------------------------------
# There now follow some tests for Boost, and OpenMP.  These
# tests are present because Drd has some regression tests that use
# these packages.  All regression test programs all compiled only
# for the primary target.  And so it is important that the configure
# checks that follow, use the correct -m32 or -m64 flag for the
# primary target (called $mflag_primary).  Otherwise, we can end up
# in a situation (eg) where, on amd64-linux, the test for Boost checks
# for usable 64-bit Boost facilities, but because we are doing a 32-bit
# only build (meaning, the primary target is x86-linux), the build
# of the regtest programs that use Boost fails, because they are 
# build as 32-bit (IN THIS EXAMPLE).
#
# Hence: ALWAYS USE $mflag_primary FOR CONFIGURE TESTS FOR FACILITIES
# NEEDED BY THE REGRESSION TEST PROGRAMS.


# Check whether the boost library 1.35 or later has been installed.
# The Boost.Threads library has undergone a major rewrite in version 1.35.0.

AC_MSG_CHECKING([for boost])

AC_LANG(C++)
safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$mflag_primary"
safe_LIBS="$LIBS"
LIBS="-lboost_thread-mt -lboost_system-mt $LIBS"

AC_LINK_IFELSE([AC_LANG_SOURCE([
#include <boost/thread.hpp>
static void thread_func(void)
{ }
int main(int argc, char** argv)
{
  boost::thread t(thread_func);
  return 0;
}
])],
[
ac_have_boost_1_35=yes
AC_SUBST([BOOST_CFLAGS], [])
AC_SUBST([BOOST_LIBS], ["-lboost_thread-mt -lboost_system-mt"])
AC_MSG_RESULT([yes])
], [
ac_have_boost_1_35=no
AC_MSG_RESULT([no])
])

LIBS="$safe_LIBS"
CXXFLAGS=$safe_CXXFLAGS
AC_LANG(C)

AM_CONDITIONAL([HAVE_BOOST_1_35], [test x$ac_have_boost_1_35 = xyes])


# does this compiler support -fopenmp, does it have the include file
# <omp.h> and does it have libgomp ?

AC_MSG_CHECKING([for OpenMP])

safe_CFLAGS=$CFLAGS
CFLAGS="-fopenmp $mflag_primary -Werror"

AC_LINK_IFELSE([AC_LANG_SOURCE([
#include <omp.h> 
int main(int argc, char** argv)
{
  omp_set_dynamic(0);
  return 0;
}
])],
[
ac_have_openmp=yes
AC_MSG_RESULT([yes])
], [
ac_have_openmp=no
AC_MSG_RESULT([no])
])
CFLAGS=$safe_CFLAGS

AM_CONDITIONAL([HAVE_OPENMP], [test x$ac_have_openmp = xyes])


# Check for __builtin_popcount
AC_MSG_CHECKING([for __builtin_popcount()])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
]], [[
  __builtin_popcount(2);
  return 0;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_BUILTIN_POPCOUT], 1,
          [Define to 1 if compiler provides __builtin_popcount().])
], [
AC_MSG_RESULT([no])
])

# Check for __builtin_clz
AC_MSG_CHECKING([for __builtin_clz()])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
]], [[
  __builtin_clz(2);
  return 0;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_BUILTIN_CLZ], 1,
          [Define to 1 if compiler provides __builtin_clz().])
], [
AC_MSG_RESULT([no])
])

# Check for __builtin_ctz
AC_MSG_CHECKING([for __builtin_ctz()])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
]], [[
  __builtin_ctz(2);
  return 0;
]])], [
AC_MSG_RESULT([yes])
AC_DEFINE([HAVE_BUILTIN_CTZ], 1,
          [Define to 1 if compiler provides __builtin_ctz().])
], [
AC_MSG_RESULT([no])
])

# does this compiler have built-in functions for atomic memory access for the
# primary target ?
AC_MSG_CHECKING([if gcc supports __sync_add_and_fetch for the primary target])

safe_CFLAGS=$CFLAGS
CFLAGS="$mflag_primary"

AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
  int variable = 1;
  return (__sync_bool_compare_and_swap(&variable, 1, 2)
          && __sync_add_and_fetch(&variable, 1) ? 1 : 0)
]])], [
  ac_have_builtin_atomic_primary=yes
  AC_MSG_RESULT([yes])
  AC_DEFINE(HAVE_BUILTIN_ATOMIC, 1, [Define to 1 if gcc supports __sync_bool_compare_and_swap() and __sync_add_and_fetch() for the primary target])
], [
  ac_have_builtin_atomic_primary=no
  AC_MSG_RESULT([no])
])

CFLAGS=$safe_CFLAGS

AM_CONDITIONAL([HAVE_BUILTIN_ATOMIC],
               [test x$ac_have_builtin_atomic_primary = xyes])


# does this compiler have built-in functions for atomic memory access for the
# secondary target ?

if test x$VGCONF_PLATFORM_SEC_CAPS != x; then

AC_MSG_CHECKING([if gcc supports __sync_add_and_fetch for the secondary target])

safe_CFLAGS=$CFLAGS
CFLAGS="$mflag_secondary"

AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
  int variable = 1;
  return (__sync_add_and_fetch(&variable, 1) ? 1 : 0)
]])], [
  ac_have_builtin_atomic_secondary=yes
  AC_MSG_RESULT([yes])
], [
  ac_have_builtin_atomic_secondary=no
  AC_MSG_RESULT([no])
])

CFLAGS=$safe_CFLAGS

fi

AM_CONDITIONAL([HAVE_BUILTIN_ATOMIC_SECONDARY],
               [test x$ac_have_builtin_atomic_secondary = xyes])

# does this compiler have built-in functions for atomic memory access on
# 64-bit integers for all targets ?

AC_MSG_CHECKING([if gcc supports __sync_add_and_fetch on uint64_t for all targets])

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <stdint.h>
]], [[
  uint64_t variable = 1;
  return __sync_add_and_fetch(&variable, 1)
]])], [
  ac_have_builtin_atomic64_primary=yes
], [
  ac_have_builtin_atomic64_primary=no
])

if test x$VGCONF_PLATFORM_SEC_CAPS != x; then

safe_CFLAGS=$CFLAGS
CFLAGS="$mflag_secondary"

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <stdint.h>
]], [[
  uint64_t variable = 1;
  return __sync_add_and_fetch(&variable, 1)
]])], [
  ac_have_builtin_atomic64_secondary=yes
], [
  ac_have_builtin_atomic64_secondary=no
])

CFLAGS=$safe_CFLAGS

fi

if test x$ac_have_builtin_atomic64_primary = xyes && \
   test x$VGCONF_PLATFORM_SEC_CAPS = x \
     -o x$ac_have_builtin_atomic64_secondary = xyes; then
  AC_MSG_RESULT([yes])
  ac_have_builtin_atomic64=yes
else
  AC_MSG_RESULT([no])
  ac_have_builtin_atomic64=no
fi

AM_CONDITIONAL([HAVE_BUILTIN_ATOMIC64],
               [test x$ac_have_builtin_atomic64 = xyes])

               
AC_MSG_CHECKING([if platform has openat2 syscall])

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <sys/syscall.h>
]], [[
  return __NR_openat2
]])], [
  ac_have_openat2=yes
  AC_MSG_RESULT([yes])
], [
  ac_have_openat2=no
  AC_MSG_RESULT([no])
])

AM_CONDITIONAL([HAVE_OPENAT2],
               [test x$ac_have_openat2 = xyes])

# check for crypto

safe_LIBS="$LIBS"
LIBS="-lcrypto"
AC_MSG_CHECKING([if platform has openssl crypto])

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <openssl/crypto.h>
]], [[
  CRYPTO_secure_malloc_init(1<<20, 8);
]])], [
  ac_have_openssl=yes
  AC_MSG_RESULT([yes])
], [
  ac_have_openssl=no
  AC_MSG_RESULT([no])
])
LIBS=$safe_LIBS

AM_CONDITIONAL([HAVE_OPENSSL],
               [test x$ac_have_openssl = xyes])

AC_MSG_CHECKING([if platform has aio_readv])

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <aio.h>
  #include <stdlib.h>
]], [[
  aio_readv(NULL);
]])], [
  ac_have_aio_readv=yes
  AC_MSG_RESULT([yes])

], [
  ac_have_aio_readv=no
  AC_MSG_RESULT([no])
])

AM_CONDITIONAL([HAVE_AIO_READV],
               [test x$ac_have_aio_readv = xyes])

# does g++ have built-in functions for atomic memory access ?
AC_MSG_CHECKING([if g++ supports __sync_add_and_fetch])

safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$mflag_primary"

AC_LANG_PUSH(C++)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
  int variable = 1;
  return (__sync_bool_compare_and_swap(&variable, 1, 2)
          && __sync_add_and_fetch(&variable, 1) ? 1 : 0)
]])], [
  ac_have_builtin_atomic_cxx=yes
  AC_MSG_RESULT([yes])
  AC_DEFINE(HAVE_BUILTIN_ATOMIC_CXX, 1, [Define to 1 if g++ supports __sync_bool_compare_and_swap() and __sync_add_and_fetch()])
], [
  ac_have_builtin_atomic_cxx=no
  AC_MSG_RESULT([no])
])
AC_LANG_POP(C++)

CXXFLAGS=$safe_CXXFLAGS

AM_CONDITIONAL([HAVE_BUILTIN_ATOMIC_CXX], [test x$ac_have_builtin_atomic_cxx = xyes])


if test x$ac_have_usable_linux_futex_h = xyes \
        -a x$ac_have_builtin_atomic_primary = xyes; then
  ac_enable_linux_ticket_lock_primary=yes
fi
AM_CONDITIONAL([ENABLE_LINUX_TICKET_LOCK_PRIMARY],
               [test x$ac_enable_linux_ticket_lock_primary = xyes])

if test x$VGCONF_PLATFORM_SEC_CAPS != x \
        -a x$ac_have_usable_linux_futex_h = xyes \
        -a x$ac_have_builtin_atomic_secondary = xyes; then
  ac_enable_linux_ticket_lock_secondary=yes
fi
AM_CONDITIONAL([ENABLE_LINUX_TICKET_LOCK_SECONDARY],
               [test x$ac_enable_linux_ticket_lock_secondary = xyes])


# does libstdc++ support annotating shared pointers ?
AC_MSG_CHECKING([if libstdc++ supports annotating shared pointers])

safe_CXXFLAGS=$CXXFLAGS
CXXFLAGS="-std=c++0x"

AC_LANG_PUSH(C++)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <memory>
]], [[
  std::shared_ptr<int> p
]])], [
  ac_have_shared_ptr=yes
], [
  ac_have_shared_ptr=no
])
if test x$ac_have_shared_ptr = xyes; then
  # If compilation of the program below fails because of a syntax error
  # triggered by substituting one of the annotation macros then that
  # means that libstdc++ supports these macros.
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[
    #define _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(a) (a)----
    #define _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(a) (a)----
    #include <memory>
  ]], [[
    std::shared_ptr<int> p
  ]])], [
    ac_have_shared_pointer_annotation=no
    AC_MSG_RESULT([no])
  ], [
    ac_have_shared_pointer_annotation=yes
    AC_MSG_RESULT([yes])
    AC_DEFINE(HAVE_SHARED_POINTER_ANNOTATION, 1,
              [Define to 1 if libstd++ supports annotating shared pointers])
  ])
else
  ac_have_shared_pointer_annotation=no
  AC_MSG_RESULT([no])
fi
AC_LANG_POP(C++)

CXXFLAGS=$safe_CXXFLAGS

AM_CONDITIONAL([HAVE_SHARED_POINTER_ANNOTATION],
               [test x$ac_have_shared_pointer_annotation = xyes])

# checking for GNU libc C17 aligned_alloc
# just check glibc version rather than trying to muck around
# checking the runtime behaviour or seeing if it is a weak alias
AC_MSG_CHECKING([for AT_GNU_LIBC_C17_ALIGNED_ALLOC])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <features.h>
]], [[
#if !defined(__GLIBC__) || __GLIBC__ != 2 || !defined(__GLIBC_MINOR__) || __GLIBC_MINOR__ < 38
#error "not GNU libc 2.38 or later"
#endif
]])], [
    AC_MSG_RESULT([yes])
    AC_DEFINE([HAVE_GNU_LIBC_C17_ALIGNED_ALLOC], 1,
              [Define to 1 if you have GNU libc C17 aligned_alloc.])

], [
    AC_MSG_RESULT([no])
])

# Check for C11 thrd_create()
AC_MSG_CHECKING([for thrd_create()])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <threads.h>
int thrd_entry(void *arg) { return 0; }
], [[thrd_t thr; return thrd_create(&thr, thrd_entry, NULL);]])],
[
ac_cxx_have_thrd_create=yes
AC_MSG_RESULT([yes])
], [
ac_cxx_have_thrd_create=no
AC_MSG_RESULT([no])
])

AM_CONDITIONAL(HAVE_THRD_CREATE, test x$ac_cxx_have_thrd_create = xyes)



#----------------------------------------------------------------------------
# Ok.  We're done checking.
#----------------------------------------------------------------------------

# Nb: VEX/Makefile is generated from Makefile.vex.in.
AC_CONFIG_FILES([
   Makefile 
   VEX/Makefile:Makefile.vex.in
   valgrind.spec
   valgrind.pc
   glibc-2.X.supp
   glibc-2.X-helgrind.supp
   glibc-2.X-drd.supp
   include/valgrind.h
   docs/Makefile 
   docs/xml/vg-entities.xml
   tests/Makefile 
   tests/vg_regtest 
   perf/Makefile 
   perf/vg_perf
   gdbserver_tests/Makefile
   gdbserver_tests/solaris/Makefile
   include/Makefile 
   auxprogs/Makefile
   mpi/Makefile
   coregrind/Makefile 
   memcheck/Makefile
   memcheck/tests/Makefile
   memcheck/tests/common/Makefile
   memcheck/tests/amd64/Makefile
   memcheck/tests/x86/Makefile
   memcheck/tests/linux/Makefile
   memcheck/tests/linux/debuginfod-check.vgtest
   memcheck/tests/darwin/Makefile
   memcheck/tests/solaris/Makefile
   memcheck/tests/freebsd/Makefile
   memcheck/tests/amd64-linux/Makefile
   memcheck/tests/arm64-linux/Makefile
   memcheck/tests/x86-linux/Makefile
   memcheck/tests/amd64-solaris/Makefile
   memcheck/tests/x86-solaris/Makefile
   memcheck/tests/amd64-freebsd/Makefile
   memcheck/tests/x86-freebsd/Makefile
   memcheck/tests/ppc32/Makefile
   memcheck/tests/ppc64/Makefile
   memcheck/tests/s390x/Makefile
   memcheck/tests/mips32/Makefile
   memcheck/tests/mips64/Makefile
   memcheck/tests/vbit-test/Makefile
   cachegrind/Makefile
   cachegrind/tests/Makefile
   cachegrind/tests/x86/Makefile
   cachegrind/cg_annotate
   cachegrind/cg_diff
   cachegrind/cg_merge
   callgrind/Makefile
   callgrind/callgrind_annotate
   callgrind/callgrind_control
   callgrind/tests/Makefile
   helgrind/Makefile
   helgrind/tests/Makefile
   drd/Makefile
   drd/scripts/download-and-build-splash2
   drd/tests/Makefile
   massif/Makefile
   massif/tests/Makefile
   massif/ms_print
   dhat/Makefile
   dhat/tests/Makefile
   lackey/Makefile
   lackey/tests/Makefile
   none/Makefile
   none/tests/Makefile
   none/tests/scripts/Makefile
   none/tests/amd64/Makefile
   none/tests/ppc32/Makefile
   none/tests/ppc64/Makefile
   none/tests/x86/Makefile
   none/tests/arm/Makefile
   none/tests/arm64/Makefile
   none/tests/s390x/Makefile
   none/tests/mips32/Makefile
   none/tests/mips64/Makefile
   none/tests/nanomips/Makefile
   none/tests/linux/Makefile
   none/tests/darwin/Makefile
   none/tests/solaris/Makefile
   none/tests/freebsd/Makefile
   none/tests/amd64-linux/Makefile
   none/tests/x86-linux/Makefile
   none/tests/amd64-darwin/Makefile
   none/tests/x86-darwin/Makefile
   none/tests/amd64-solaris/Makefile
   none/tests/x86-solaris/Makefile
   none/tests/x86-freebsd/Makefile
   exp-bbv/Makefile
   exp-bbv/tests/Makefile
   exp-bbv/tests/x86/Makefile
   exp-bbv/tests/x86-linux/Makefile
   exp-bbv/tests/amd64-linux/Makefile
   exp-bbv/tests/ppc32-linux/Makefile
   exp-bbv/tests/arm-linux/Makefile
   shared/Makefile
   solaris/Makefile
])
AC_CONFIG_FILES([coregrind/link_tool_exe_linux],
                [chmod +x coregrind/link_tool_exe_linux])
AC_CONFIG_FILES([coregrind/link_tool_exe_freebsd],
                [chmod +x coregrind/link_tool_exe_freebsd])
AC_CONFIG_FILES([coregrind/link_tool_exe_darwin],
                [chmod +x coregrind/link_tool_exe_darwin])
AC_CONFIG_FILES([coregrind/link_tool_exe_solaris],
                [chmod +x coregrind/link_tool_exe_solaris])
AC_CONFIG_FILES([tests/filter_stderr_basic],
                [chmod +x tests/filter_stderr_basic])
AC_CONFIG_FILES([tests/filter_discards],
                [chmod +x tests/filter_discards])
AC_CONFIG_FILES([memcheck/tests/filter_stderr],
                [chmod +x memcheck/tests/filter_stderr])
AC_CONFIG_FILES([memcheck/tests/filter_dw4],
                [chmod +x memcheck/tests/filter_dw4])
AC_CONFIG_FILES([memcheck/tests/filter_overlaperror],
                [chmod +x memcheck/tests/filter_overlaperror])
AC_CONFIG_FILES([memcheck/tests/x86/filter_pushfpopf],
                [chmod +x memcheck/tests/x86/filter_pushfpopf])
AC_CONFIG_FILES([gdbserver_tests/filter_gdb],
                [chmod +x gdbserver_tests/filter_gdb])
AC_CONFIG_FILES([gdbserver_tests/filter_memcheck_monitor],
                [chmod +x gdbserver_tests/filter_memcheck_monitor])
AC_CONFIG_FILES([gdbserver_tests/filter_stderr],
                [chmod +x gdbserver_tests/filter_stderr])
AC_CONFIG_FILES([gdbserver_tests/filter_vgdb],
                [chmod +x gdbserver_tests/filter_vgdb])
AC_CONFIG_FILES([drd/tests/filter_stderr],
                [chmod +x drd/tests/filter_stderr])
AC_CONFIG_FILES([drd/tests/filter_error_count],
                [chmod +x drd/tests/filter_error_count])
AC_CONFIG_FILES([drd/tests/filter_error_summary],
                [chmod +x drd/tests/filter_error_summary])
AC_CONFIG_FILES([drd/tests/filter_stderr_and_thread_no_and_offset],
                [chmod +x drd/tests/filter_stderr_and_thread_no_and_offset])
AC_CONFIG_FILES([drd/tests/filter_thread_no],
                [chmod +x drd/tests/filter_thread_no])
AC_CONFIG_FILES([drd/tests/filter_xml_and_thread_no],
                [chmod +x drd/tests/filter_xml_and_thread_no])
AC_CONFIG_FILES([helgrind/tests/filter_stderr],
                [chmod +x helgrind/tests/filter_stderr])
AC_OUTPUT

cat<<EOF

                    Version: ${VERSION}
         Maximum build arch: ${ARCH_MAX}
         Primary build arch: ${VGCONF_ARCH_PRI}
       Secondary build arch: ${VGCONF_ARCH_SEC}
                   Build OS: ${VGCONF_OS}
     Link Time Optimisation: ${vg_cv_lto}
       Primary build target: ${VGCONF_PLATFORM_PRI_CAPS}
     Secondary build target: ${VGCONF_PLATFORM_SEC_CAPS}
           Platform variant: ${VGCONF_PLATVARIANT}
      Primary -DVGPV string: -DVGPV_${VGCONF_ARCH_PRI}_${VGCONF_OS}_${VGCONF_PLATVARIANT}=1
         Default supp files: ${DEFAULT_SUPP}

EOF
