
INCLUDES = include/basictypes.h

##OBJS = basictypes.o ir_defs.o arena.o linker.o dispatch.o
OBJS = basictypes.o ir_defs.o

CC_OPTS = -g -Wall -Iinclude

all: $(OBJS)
	gcc $(CC_OPTS) -o vex $(OBJS)

clean:
	rm -f *.o vex

basictypes.o: basictypes.c $(INCLUDES)
	gcc $(CC_OPTS) -c basictypes.c
ir_defs.o: ir_defs.c $(INCLUDES)
	gcc $(CC_OPTS) -c ir_defs.c
arena.o: arena.c $(INCLUDES)
	gcc $(CC_OPTS) -c arena.c
linker.o: linker.c $(INCLUDES)
	gcc $(CC_OPTS) -c linker.c
dispatch.o: dispatch.c $(INCLUDES)
	gcc $(CC_OPTS) -c dispatch.c


