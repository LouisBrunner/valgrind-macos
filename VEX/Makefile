
PUB_HEADERS = 	pub/libvex_basictypes.h 		\
		pub/libvex_ir.h				\
		pub/libvex.h				\
		pub/libvex_guest_x86.h

PRIV_HEADERS = 	priv/host-x86/hdefs.h			\
		priv/host-generic/h_generic_regs.h	\
		priv/main/vex_globals.h			\
		priv/main/vex_util.h			\
		priv/guest-x86/gdefs.h			\
		priv/ir/iropt.h

LIB_OBJS = 	priv/ir/irdefs.o			\
		priv/ir/iropt.o				\
		priv/main/vex_main.o			\
		priv/main/vex_globals.o			\
		priv/main/vex_util.o			\
		priv/host-x86/hdefs.o			\
		priv/host-x86/isel.o			\
		priv/host-generic/h_generic_regs.o	\
		priv/host-generic/reg_alloc.o		\
		priv/guest-x86/ghelpers.o		\
		priv/guest-x86/toIR.o

PUB_INCLUDES = -Ipub

# Do not add any priv/host-ARCH or priv/guest-ARCH directories to this
# list, as they contain duplicate file names (each host has a hdefs.h,
# for example).
PRIV_INCLUDES = -Ipriv


CC = gcc341
CCFLAGS = -g -O -Wall -Wshadow -Winline $(EXTRA_CFLAGS)

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
	rm -f hacked104/valgrind.so
	(cd hacked104 && make install)

vex: libvex.a test_main.o
	$(CC) $(CCFLAGS) -o vex test_main.o libvex.a

libvex.a: $(LIB_OBJS)
	rm -f libvex.a
	ar clq libvex.a $(LIB_OBJS)

clean:
	rm -f $(LIB_OBJS) libvex.a vex test_main.o
	(cd hacked104 && make clean)



ALL_HEADERS  = $(PUB_HEADERS) $(PRIV_HEADERS)
ALL_INCLUDES = $(PUB_INCLUDES) $(PRIV_INCLUDES)

test_main.o: $(PUB_HEADERS) test_main.c
	$(CC) $(CCFLAGS) $(PUB_INCLUDES) -o test_main.o \
					 -c test_main.c

priv/ir/irdefs.o: $(ALL_HEADERS) priv/ir/irdefs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/ir/irdefs.o \
					 -c priv/ir/irdefs.c

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

priv/host-x86/isel.o: $(ALL_HEADERS) priv/host-x86/isel.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-x86/isel.o \
					 -c priv/host-x86/isel.c

priv/host-generic/h_generic_regs.o: $(ALL_HEADERS) priv/host-generic/h_generic_regs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/h_generic_regs.o \
					 -c priv/host-generic/h_generic_regs.c

priv/host-generic/reg_alloc.o: $(ALL_HEADERS) priv/host-generic/reg_alloc.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/reg_alloc.o \
					 -c priv/host-generic/reg_alloc.c

priv/guest-x86/toIR.o: $(ALL_HEADERS) priv/guest-x86/toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/toIR.o \
					 -c priv/guest-x86/toIR.c

priv/guest-x86/ghelpers.o: $(ALL_HEADERS) priv/guest-x86/ghelpers.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/ghelpers.o \
					 -c priv/guest-x86/ghelpers.c
