
INCLUDES = include/arena.h				\
	   include/basictypes.h include/ir_defs.h 	\
	   include/host_regs.h include/x86h_defs.h

##OBJS = basictypes.o ir_defs.o arena.o linker.o dispatch.o
OBJS = basictypes.o ir_defs.o host_regs.o \
	x86h_defs.o isel_x86.o reg_alloc.o test_main.o

GCC = gcc341
##GCC = gcc
CC_OPTS = -g -Wall -Wshadow -Iinclude

all: $(OBJS)
	$(GCC) $(CC_OPTS) -o vex $(OBJS)

clean:
	rm -f *.o vex

basictypes.o: basictypes.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c basictypes.c
ir_defs.o: ir_defs.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c ir_defs.c
host_regs.o: host_regs.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c host_regs.c
x86h_defs.o: x86h_defs.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c x86h_defs.c
isel_x86.o: isel_x86.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c isel_x86.c
reg_alloc.o: reg_alloc.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c reg_alloc.c
arena.o: arena.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c arena.c
linker.o: linker.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c linker.c
dispatch.o: dispatch.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c dispatch.c

test_main.o: test_main.c $(INCLUDES)
	$(GCC) $(CC_OPTS) -c test_main.c


