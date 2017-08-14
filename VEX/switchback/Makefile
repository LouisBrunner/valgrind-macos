
all: switchback.c linker.c linker.h
	gcc -Wall -O -g -o switchback switchback.c linker.c ../libvex.a

test_ppc:
	gcc -Wall -O -c test_ppc_jm1.c -mregnames

clean:
	rm -f switchback switchback.o linker.o
