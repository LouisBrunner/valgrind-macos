
PUB_HEADERS = 	pub/libvex_basictypes.h 		\
		pub/libvex_ir.h				\
		pub/libvex.h

PRIV_HEADERS = 	priv/host-x86/x86h_defs.h		\
		priv/host-generic/host_regs.h		\
		priv/main/vex_globals.h			\
		priv/main/vex_util.h			\
		priv/guest-x86/x86guest_defs.h

LIB_OBJS = 	priv/ir/ir_defs.o			\
		priv/main/vex_main.o			\
		priv/main/vex_globals.o			\
		priv/main/vex_util.o			\
		priv/host-x86/x86h_defs.o		\
		priv/host-x86/isel_x86.o		\
		priv/host-generic/host_regs.o		\
		priv/host-generic/reg_alloc.o		\
		priv/guest-x86/x86toIR.o

PUB_INCLUDES = -Ipub

PRIV_INCLUDES = -Ipriv/ir -Ipriv/main -Ipriv/host-generic \
		-Ipriv/host-x86 -Ipriv/guest-x86

APP_OBJS =	test_main.o


CC = gcc341
CCFLAGS = -g -Wall -Wshadow

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

all: libvex.a $(APP_OBJS)
	$(CC) $(CCFLAGS) -o vex $(APP_OBJS) libvex.a

libvex.a: $(LIB_OBJS)
	rm -f libvex.a
	ar clq libvex.a $(LIB_OBJS)

clean:
	rm -f $(APP_OBJS) $(LIB_OBJS) libvex.a vex




ALL_HEADERS  = $(PUB_HEADERS) $(PRIV_HEADERS)
ALL_INCLUDES = $(PUB_INCLUDES) $(PRIV_INCLUDES)

test_main.o: $(PUB_HEADERS) test_main.c
	$(CC) $(CCFLAGS) $(PUB_INCLUDES) -o test_main.o \
					 -c test_main.c

priv/ir/ir_defs.o: $(ALL_HEADERS) priv/ir/ir_defs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/ir/ir_defs.o \
					 -c priv/ir/ir_defs.c

priv/main/vex_main.o: $(ALL_HEADERS) priv/main/vex_main.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/vex_main.o \
					 -c priv/main/vex_main.c

priv/main/vex_globals.o: $(ALL_HEADERS) priv/main/vex_globals.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/vex_globals.o \
					 -c priv/main/vex_globals.c

priv/main/vex_util.o: $(ALL_HEADERS) priv/main/vex_util.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/vex_util.o \
					 -c priv/main/vex_util.c

priv/host-x86/x86h_defs.o: $(ALL_HEADERS) priv/host-x86/x86h_defs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-x86/x86h_defs.o \
					 -c priv/host-x86/x86h_defs.c

priv/host-x86/isel_x86.o: $(ALL_HEADERS) priv/host-x86/isel_x86.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-x86/isel_x86.o \
					 -c priv/host-x86/isel_x86.c

priv/host-generic/host_regs.o: $(ALL_HEADERS) priv/host-generic/host_regs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/host_regs.o \
					 -c priv/host-generic/host_regs.c

priv/host-generic/reg_alloc.o: $(ALL_HEADERS) priv/host-generic/reg_alloc.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/host-generic/reg_alloc.o \
					 -c priv/host-generic/reg_alloc.c

priv/guest-x86/x86toIR.o: $(ALL_HEADERS) priv/guest-x86/x86toIR.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/guest-x86/x86toIR.o \
					 -c priv/guest-x86/x86toIR.c