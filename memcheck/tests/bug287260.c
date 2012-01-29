#include <stdio.h>

typedef struct {
	unsigned int stuff : 21;
	signed int rotation : 10;
} Oink;

int
main (int argc, char **argv)
{
	volatile Oink r;

	r.rotation = 45;
	fprintf (stderr, "%d\n", r.rotation);
	return 0;
}
