
all: switchback.c linker.c linker.h
	gcc -m64 -Wall -O -g -o switchback switchback.c linker.c ../libvex_ppc64_linux.a
#	gcc -Wall -O -g -o switchback switchback.c linker.c ../libvex.a

test_ppc:
	gcc -Wall -m64  -mregnames -O -c test_ppc_jm1.c

clean:
	rm -f switchback switchback.o linker.o
