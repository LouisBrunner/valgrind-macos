
PUB_HEADERS = 	pub/libjit_basictypes.h 		\
		pub/libjit_ir.h				\
		pub/libjit.h

PRIV_HEADERS = 	priv/ir/ir_defs.h			\
		priv/host-x86/x86h_defs.h		\
		priv/host-generic/host_regs.h

LIB_OBJS = 	priv/ir/ir_defs.o			\
		priv/host-x86/x86h_defs.o		\
		priv/host-x86/isel_x86.o		\
		priv/host-generic/host_regs.o		\
		priv/host-generic/reg_alloc.o

PUB_INCLUDES = -Ipub

PRIV_INCLUDES = -Ipub -Ipriv/ir -Ipriv/host-generic -Ipriv/host-x86

APP_OBJS =	test_main.o


CC = gcc341
CCFLAGS = -g -Wall -Wshadow

all: libjit.a $(APP_OBJS)
	$(CC) $(CCFLAGS) -o vex $(APP_OBJS) libjit.a

libjit.a: $(LIB_OBJS)
	ar clq libjit.a $(LIB_OBJS)

%.o: %.c
	$(CC) $(CCFLAGS) $(PRIV_INCLUDES) -c -o $@ $<

clean:
	rm -f $(APP_OBJS) $(LIB_OBJS) libjit.a vex



