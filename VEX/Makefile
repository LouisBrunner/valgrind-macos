
PUB_HEADERS = 	pub/libvex_basictypes.h 		\
		pub/libvex_ir.h				\
		pub/libvex.h				\
		pub/libvex_trc_values.h			\
		pub/libvex_emwarn.h			\
		pub/libvex_guest_x86.h			\
		pub/libvex_guest_amd64.h		\
		pub/libvex_guest_arm.h

PRIV_HEADERS = 	priv/host-x86/hdefs.h			\
		priv/host-arm/hdefs.h			\
		priv/host-generic/h_generic_regs.h	\
		priv/main/vex_globals.h			\
		priv/main/vex_util.h			\
		priv/guest-x86/gdefs.h			\
		priv/guest-arm/gdefs.h			\
		priv/ir/irmatch.h			\
		priv/ir/iropt.h

LIB_OBJS = 	priv/ir/irdefs.o			\
		priv/ir/irmatch.o			\
		priv/ir/iropt.o				\
		priv/main/vex_main.o			\
		priv/main/vex_globals.o			\
		priv/main/vex_util.o			\
		priv/host-x86/hdefs.o			\
		priv/host-arm/hdefs.o			\
		priv/host-x86/isel.o			\
		priv/host-arm/isel.o			\
		priv/host-generic/h_generic_regs.o	\
		priv/host-generic/reg_alloc2.o		\
		priv/guest-x86/ghelpers.o		\
		priv/guest-amd64/ghelpers.o		\
		priv/guest-arm/ghelpers.o		\
		priv/guest-x86/toIR.o			\
		priv/guest-arm/toIR.o

PUB_INCLUDES = -Ipub

# Do not add any priv/host-ARCH or priv/guest-ARCH directories to this
# list, as they contain duplicate file names (each host has a hdefs.h,
# for example).
PRIV_INCLUDES = -Ipriv


CC = gcc
CCFLAGS = -g -O -Wall -Wmissing-prototypes -Wshadow -Winline $(EXTRA_CFLAGS)

#CC = icc
#CCFLAGS = -g  -Wbrief -Wall -wd981 -wd279 -wd1287 -wd869 \
#	  -wd810 -wd1419 -wd181 -wd111 -wd167
# 981: operands are evaluated in unspecified order
# 279: controlling expression is constant
# 1287: invalid attribute for parameter
# 869: parameter "..." was never referenced
# 810: conversion from "int" to "Char={char}" may lose significant bits
# 181: argument is incompatible with corresponding format string conversion
# 111: statement is unreachable
# 167: argument of type unsigned char incompatible with formal of type char
# (the above are for icc 8.0 -- 8.0.0.55 I think)

all: libvex.a
	rm -f head20041019/coregrind/stage2
	(cd head20041019/coregrind && make --quiet install)
	(cd head20041019/none && make --quiet install)
	(cd head20041019/lackey && make --quiet install)
	(cd head20041019/addrcheck && make --quiet install)
	(cd head20041019/memcheck && make --quiet install)

vex: libvex.a test_main.o
	$(CC) $(CCFLAGS) -o vex test_main.o libvex.a

libvex.a: $(LIB_OBJS)
	rm -f libvex.a
	ar clq libvex.a $(LIB_OBJS)

clean:
	rm -f $(LIB_OBJS) libvex.a vex test_main.o
	(cd head20041019 && make --quiet clean)


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

priv/main/vex_main.o: $(ALL_HEADERS) priv/main/vex_main.c
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

priv/host-arm/hdefs.o: $(ALL_HEADERS) priv/host-arm/hdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-arm/hdefs.o \
					 -c priv/host-arm/hdefs.c

priv/host-x86/isel.o: $(ALL_HEADERS) priv/host-x86/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-x86/isel.o \
					 -c priv/host-x86/isel.c

priv/host-arm/isel.o: $(ALL_HEADERS) priv/host-arm/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-arm/isel.o \
					 -c priv/host-arm/isel.c

priv/host-generic/h_generic_regs.o: $(ALL_HEADERS) priv/host-generic/h_generic_regs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/h_generic_regs.o \
					 -c priv/host-generic/h_generic_regs.c

priv/host-generic/reg_alloc2.o: $(ALL_HEADERS) priv/host-generic/reg_alloc2.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/reg_alloc2.o \
					 -c priv/host-generic/reg_alloc2.c

priv/guest-x86/toIR.o: $(ALL_HEADERS) priv/guest-x86/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/toIR.o \
					 -c priv/guest-x86/toIR.c

priv/guest-x86/ghelpers.o: $(ALL_HEADERS) priv/guest-x86/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/ghelpers.o \
					 -c priv/guest-x86/ghelpers.c

priv/guest-amd64/ghelpers.o: $(ALL_HEADERS) priv/guest-amd64/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-amd64/ghelpers.o \
					 -c priv/guest-amd64/ghelpers.c

priv/guest-arm/ghelpers.o: $(ALL_HEADERS) priv/guest-arm/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-arm/ghelpers.o \
					 -c priv/guest-arm/ghelpers.c

priv/guest-arm/toIR.o: $(ALL_HEADERS) priv/guest-arm/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-arm/toIR.o \
					 -c priv/guest-arm/toIR.c
