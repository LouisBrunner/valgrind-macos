
INCLUDES = include/basictypes.h

OBJS = basictypes.o arena.o linker.o dispatch.o

CC_OPTS = -g -Wall -Iinclude

all: $(OBJS)
	gcc $(CC_OPTS) -o vex $(OBJS)

clean:
	rm -f *.o vex

basictypes.o: basictypes.c $(INCLUDES)
	gcc $(CC_OPTS) -c basictypes.c
arena.o: arena.c $(INCLUDES)
	gcc $(CC_OPTS) -c arena.c
linker.o: linker.c $(INCLUDES)
	gcc $(CC_OPTS) -c linker.c
dispatch.o: dispatch.c $(INCLUDES)
	gcc $(CC_OPTS) -c dispatch.c


