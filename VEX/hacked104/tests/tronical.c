#include <stdio.h>

struct Foo
{
    int a1 : 1;
    int a2 : 1;
    int a3 : 1;
    int a4 : 1;
    int a5 : 1;
    int a6 : 1;
    int a7 : 1;
    int bleh : 1;
};

struct Foo* foo;

void set()
{
    foo->bleh = 1;
}

void get()
{
    if ( foo->bleh == 0 )
	printf( "blieb\n" );
}

int main()
{
  foo = malloc(sizeof(struct Foo));
    set();

    get();

    return 0;
}

