# Crude makefile to build the "vex" executable from test_main.c

vex: test_main.c test_main.h ../pub/*.h ../priv/*.c ../priv/*.h
	(cd ..; make -f Makefile-gcc)
	cc -I../pub -o vex test_main.c ../libvex.a

clean:
	rm -f vex ../priv/*.o
