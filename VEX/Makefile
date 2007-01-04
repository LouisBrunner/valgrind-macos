
PUB_HEADERS = 	pub/libvex_basictypes.h 		\
		pub/libvex_ir.h				\
		pub/libvex.h				\
		pub/libvex_trc_values.h			\
		pub/libvex_emwarn.h			\
		pub/libvex_guest_x86.h			\
		pub/libvex_guest_amd64.h		\
		pub/libvex_guest_arm.h			\
		pub/libvex_guest_ppc32.h		\
		pub/libvex_guest_ppc64.h		\
		pub/libvex_guest_offsets.h

PRIV_HEADERS = 	priv/host-x86/hdefs.h			\
		priv/host-amd64/hdefs.h			\
		priv/host-arm/hdefs.h			\
		priv/host-ppc/hdefs.h			\
		priv/host-generic/h_generic_regs.h	\
		priv/host-generic/h_generic_simd64.h	\
		priv/main/vex_globals.h			\
		priv/main/vex_util.h			\
		priv/guest-generic/g_generic_x87.h	\
		priv/guest-generic/bb_to_IR.h		\
		priv/guest-x86/gdefs.h			\
		priv/guest-amd64/gdefs.h		\
		priv/guest-arm/gdefs.h			\
		priv/guest-ppc/gdefs.h			\
		priv/ir/irmatch.h			\
		priv/ir/iropt.h

LIB_OBJS = 	priv/ir/irdefs.o			\
		priv/ir/irmatch.o			\
		priv/ir/iropt.o				\
		priv/main/vex_main.o			\
		priv/main/vex_globals.o			\
		priv/main/vex_util.o			\
		priv/host-x86/hdefs.o			\
		priv/host-amd64/hdefs.o			\
		priv/host-arm/hdefs.o			\
		priv/host-ppc/hdefs.o			\
		priv/host-x86/isel.o			\
		priv/host-amd64/isel.o			\
		priv/host-arm/isel.o			\
		priv/host-ppc/isel.o			\
		priv/host-generic/h_generic_regs.o	\
		priv/host-generic/h_generic_simd64.o	\
		priv/host-generic/reg_alloc2.o		\
		priv/guest-generic/g_generic_x87.o	\
		priv/guest-generic/bb_to_IR.o		\
		priv/guest-x86/ghelpers.o		\
		priv/guest-amd64/ghelpers.o		\
		priv/guest-arm/ghelpers.o		\
		priv/guest-ppc/ghelpers.o		\
		priv/guest-x86/toIR.o			\
		priv/guest-amd64/toIR.o			\
		priv/guest-arm/toIR.o			\
		priv/guest-ppc/toIR.o

PUB_INCLUDES = -Ipub

# Do not add any priv/host-ARCH or priv/guest-ARCH directories to this
# list, as they contain duplicate file names (each host has a hdefs.h,
# for example).
PRIV_INCLUDES = -Ipriv


ifndef CC
   CC = gcc 
endif 
ifndef AR
   AR = ar 
endif

# Put -g -O2 after any flags we inherit from V.  -O2 vs -O
# makes a significant difference, at least with gcc4.
CCFLAGS = -Wall -Wmissing-prototypes -Wshadow -Winline \
		-Wpointer-arith -Wbad-function-cast -Wcast-qual \
		-Wcast-align -Wmissing-declarations \
		$(EXTRA_CFLAGS) -g -O2

#CC = icc
#CCFLAGS = -g -Wall -wd981 -wd279 -wd1287 -wd869 -wd111 -wd188 -wd186
# 981: operands are evaluated in unspecified order
# 279: controlling expression is constant
# 1287: invalid attribute for parameter
# 869: parameter "..." was never referenced
# 111: statement is unreachable
# 188: enumerated type mixed with another type
# (the above are for icc 8.0 -- 8.0.0.55 I think)
# 186: pointless comparison of unsigned integer with zero

# kludge: stops V biarch builds screwing up at -j 2 or above
# The Right fix is to autoconf/automake-ise vex.
.NOTPARALLEL:

all: vex

# Empty, needed for Valgrind
install:

scratch: clean version all

vex: libvex.a test_main.o
	$(CC) $(CCFLAGS) -o vex test_main.o libvex.a

libvex.a: $(LIB_OBJS)
	rm -f libvex.a
	$(AR) clq libvex.a $(LIB_OBJS)


# The idea with these TAG_s is to mark the flavour of libvex.a 
# most recently built, so if the same target is re-requested, we
# don't rebuild everything, but if a different one is requested
# then we scrub everything and start over.

libvex_x86_linux.a: TAG_x86_linux libvex.a
	mv -f libvex.a libvex_x86_linux.a
TAG_x86_linux:
	if [ ! -f TAG_x86_linux ] ; then rm -f $(LIB_OBJS) TAG_* libvex.a ; fi
	touch TAG_x86_linux

libvex_amd64_linux.a: TAG_amd64_linux libvex.a
	mv -f libvex.a libvex_amd64_linux.a
TAG_amd64_linux:
	if [ ! -f TAG_amd64_linux ] ; then rm -f $(LIB_OBJS) TAG_* libvex.a ; fi
	touch TAG_amd64_linux

libvex_ppc32_linux.a: TAG_ppc32_linux libvex.a
	mv -f libvex.a libvex_ppc32_linux.a
TAG_ppc32_linux:
	if [ ! -f TAG_ppc32_linux ] ; then rm -f $(LIB_OBJS) TAG_* libvex.a ; fi
	touch TAG_ppc32_linux

libvex_ppc64_linux.a: TAG_ppc64_linux libvex.a
	mv -f libvex.a libvex_ppc64_linux.a
TAG_ppc64_linux:
	if [ ! -f TAG_ppc64_linux ] ; then rm -f $(LIB_OBJS) TAG_* libvex.a ; fi
	touch TAG_ppc64_linux

libvex_ppc32_aix5.a: TAG_ppc32_aix5 libvex.a
	mv -f libvex.a libvex_ppc32_aix5.a
TAG_ppc32_aix5:
	if [ ! -f TAG_ppc32_aix5 ] ; then rm -f $(LIB_OBJS) TAG_* libvex.a ; fi
	touch TAG_ppc32_aix5

libvex_ppc64_aix5.a: TAG_ppc64_aix5 libvex.a
	mv -f libvex.a libvex_ppc64_aix5.a
TAG_ppc64_aix5:
	if [ ! -f TAG_ppc64_aix5 ] ; then rm -f $(LIB_OBJS) TAG_* libvex.a ; fi
	touch TAG_ppc64_aix5


# This doesn't get rid of priv/main/vex_svnversion.h, because
# that can't be regenerated in the final Valgrind tarball, and
# so if 'make clean' did get rid of it, then in the tarball,
# doing 'make ; make clean ; make' (or distclean) would fail.
clean:
	rm -f $(LIB_OBJS) *.a vex test_main.o TAG_* \
		pub/libvex_guest_offsets.h

version:
	rm -f priv/main/vex_svnversion.h
	cat quote.txt   >> priv/main/vex_svnversion.h
	svnversion -n . >> priv/main/vex_svnversion.h
	cat quote.txt   >> priv/main/vex_svnversion.h
	cat newline.txt >> priv/main/vex_svnversion.h

minidist: version
	rm -f vex--minidist-2005MMDD.tar
	tar cf vex--minidist-2005MMDD.tar $(PUB_HEADERS) $(PRIV_HEADERS) \
		priv/main/vex_svnversion.h			\
		test_main.c test_main.h				\
		Makefile					\
		`echo $(LIB_OBJS) | sed "s/\.o/\.c/g"`
	@echo 
	@echo minidist done, size and svnversion follow:
	@ls -l vex--minidist-2005MMDD.tar
	@cat priv/main/vex_svnversion.h
	@echo

pub/libvex_guest_offsets.h:
	$(CC) -Wall -g -o auxprogs/genoffsets auxprogs/genoffsets.c
	./auxprogs/genoffsets > pub/libvex_guest_offsets.h


ALL_HEADERS  = $(PUB_HEADERS) $(PRIV_HEADERS)
ALL_INCLUDES = $(PUB_INCLUDES) $(PRIV_INCLUDES)

test_main.o: $(PUB_HEADERS) test_main.c test_main.h
	$(CC) $(CCFLAGS) $(PUB_INCLUDES) -o test_main.o \
					 -c test_main.c

priv/ir/irdefs.o: $(ALL_HEADERS) priv/ir/irdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/ir/irdefs.o \
					 -c priv/ir/irdefs.c

priv/ir/irmatch.o: $(ALL_HEADERS) priv/ir/irmatch.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/ir/irmatch.o \
					 -c priv/ir/irmatch.c

priv/ir/iropt.o: $(ALL_HEADERS) priv/ir/iropt.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/ir/iropt.o \
					 -c priv/ir/iropt.c

priv/main/vex_main.o: $(ALL_HEADERS) priv/main/vex_main.c \
					priv/main/vex_svnversion.h
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/vex_main.o \
					 -c priv/main/vex_main.c

priv/main/vex_globals.o: $(ALL_HEADERS) priv/main/vex_globals.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/vex_globals.o \
					 -c priv/main/vex_globals.c

priv/main/vex_util.o: $(ALL_HEADERS) priv/main/vex_util.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/vex_util.o \
					 -c priv/main/vex_util.c

priv/host-x86/hdefs.o: $(ALL_HEADERS) priv/host-x86/hdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-x86/hdefs.o \
					 -c priv/host-x86/hdefs.c

priv/host-amd64/hdefs.o: $(ALL_HEADERS) priv/host-amd64/hdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-amd64/hdefs.o \
					 -c priv/host-amd64/hdefs.c

priv/host-arm/hdefs.o: $(ALL_HEADERS) priv/host-arm/hdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-arm/hdefs.o \
					 -c priv/host-arm/hdefs.c

priv/host-ppc/hdefs.o: $(ALL_HEADERS) priv/host-ppc/hdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-ppc/hdefs.o \
					 -c priv/host-ppc/hdefs.c

priv/host-x86/isel.o: $(ALL_HEADERS) priv/host-x86/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-x86/isel.o \
					 -c priv/host-x86/isel.c

priv/host-amd64/isel.o: $(ALL_HEADERS) priv/host-amd64/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-amd64/isel.o \
					 -c priv/host-amd64/isel.c

priv/host-arm/isel.o: $(ALL_HEADERS) priv/host-arm/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-arm/isel.o \
					 -c priv/host-arm/isel.c

priv/host-ppc/isel.o: $(ALL_HEADERS) priv/host-ppc/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-ppc/isel.o \
					 -c priv/host-ppc/isel.c

priv/host-generic/h_generic_regs.o: $(ALL_HEADERS) priv/host-generic/h_generic_regs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/h_generic_regs.o \
					 -c priv/host-generic/h_generic_regs.c

priv/host-generic/h_generic_simd64.o: $(ALL_HEADERS) priv/host-generic/h_generic_simd64.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/h_generic_simd64.o \
					 -c priv/host-generic/h_generic_simd64.c

priv/host-generic/reg_alloc2.o: $(ALL_HEADERS) priv/host-generic/reg_alloc2.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/reg_alloc2.o \
					 -c priv/host-generic/reg_alloc2.c

priv/guest-x86/toIR.o: $(ALL_HEADERS) priv/guest-x86/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/toIR.o \
					 -c priv/guest-x86/toIR.c

priv/guest-generic/g_generic_x87.o: $(ALL_HEADERS) priv/guest-generic/g_generic_x87.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-generic/g_generic_x87.o \
					 -c priv/guest-generic/g_generic_x87.c

priv/guest-generic/bb_to_IR.o: $(ALL_HEADERS) priv/guest-generic/bb_to_IR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-generic/bb_to_IR.o \
					 -c priv/guest-generic/bb_to_IR.c

priv/guest-x86/ghelpers.o: $(ALL_HEADERS) priv/guest-x86/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/ghelpers.o \
					 -c priv/guest-x86/ghelpers.c

priv/guest-amd64/ghelpers.o: $(ALL_HEADERS) priv/guest-amd64/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-amd64/ghelpers.o \
					 -c priv/guest-amd64/ghelpers.c

priv/guest-amd64/toIR.o: $(ALL_HEADERS) priv/guest-amd64/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-amd64/toIR.o \
					 -c priv/guest-amd64/toIR.c

priv/guest-arm/ghelpers.o: $(ALL_HEADERS) priv/guest-arm/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-arm/ghelpers.o \
					 -c priv/guest-arm/ghelpers.c

priv/guest-arm/toIR.o: $(ALL_HEADERS) priv/guest-arm/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-arm/toIR.o \
					 -c priv/guest-arm/toIR.c

priv/guest-ppc/ghelpers.o: $(ALL_HEADERS) priv/guest-ppc/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-ppc/ghelpers.o \
					 -c priv/guest-ppc/ghelpers.c

priv/guest-ppc/toIR.o: $(ALL_HEADERS) priv/guest-ppc/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-ppc/toIR.o \
					 -c priv/guest-ppc/toIR.c
