
PUB_HEADERS = 	pub/libjit_basictypes.h 		\
		pub/libjit_ir.h				\
		pub/libjit.h

PRIV_HEADERS = 	priv/host-x86/x86h_defs.h		\
		priv/host-generic/host_regs.h		\
		priv/main/jit_globals.h			\
		priv/main/vex_util.h

LIB_OBJS = 						\
		priv/ir/ir_defs.o			\
		priv/main/jit_main.o			\
		priv/main/jit_globals.o			\
		priv/main/vex_util.o			\
		priv/host-x86/x86h_defs.o		\
		priv/host-x86/isel_x86.o		\
		priv/host-generic/host_regs.o		\
		priv/host-generic/reg_alloc.o

PUB_INCLUDES = -Ipub

PRIV_INCLUDES = -Ipriv/ir -Ipriv/main -Ipriv/host-generic -Ipriv/host-x86

APP_OBJS =	test_main.o


CC = gcc341
CCFLAGS = -g -Wall -Wshadow

all: libjit.a $(APP_OBJS)
	$(CC) $(CCFLAGS) -o vex $(APP_OBJS) libjit.a

libjit.a: $(LIB_OBJS)
	ar clq libjit.a $(LIB_OBJS)

clean:
	rm -f $(APP_OBJS) $(LIB_OBJS) libjit.a vex


ALL_HEADERS  = $(PUB_HEADERS) $(PRIV_HEADERS)
ALL_INCLUDES = $(PUB_INCLUDES) $(PRIV_INCLUDES)

priv/ir/ir_defs.o: $(ALL_HEADERS) priv/ir/ir_defs.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/ir/ir_defs.o \
					 -c priv/ir/ir_defs.c

priv/main/jit_main.o: $(ALL_HEADERS) priv/main/jit_main.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/jit_main.o \
					 -c priv/main/jit_main.c

priv/main/jit_globals.o: $(ALL_HEADERS) priv/main/jit_globals.c
	$(CC) $(CCFLAGS) $(ALL_INCLUDES) -o priv/main/jit_globals.o \
					 -c priv/main/jit_globals.c

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

