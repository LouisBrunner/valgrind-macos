
/* This is a modified version of the file "arena.c" from 
   "C Interfaces and Implementations", by David R. Hanson.
   The license is below.
*/
/* 

The author of this software is David R. Hanson.

Copyright (c) 1994,1995,1996,1997 by David R. Hanson. All Rights Reserved.

Permission to use, copy, modify, and distribute this software for any
purpose, subject to the provisions described below, without fee is
hereby granted, provided that this entire notice is included in all
copies of any software that is or includes a copy or modification of
this software and in all copies of the supporting documentation for
such software.

THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
WARRANTY. IN PARTICULAR, THE AUTHOR DOES MAKE ANY REPRESENTATION OR
WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
ITS FITNESS FOR ANY PARTICULAR PURPOSE.

David Hanson / drh@microsoft.com / http://www.research.microsoft.com/~drh/
$Id: CPYRIGHT,v 1.2 1997/11/04 22:31:40 drh Exp $
*/


//static char rcsid[] = "$Id: H:/drh/idioms/book/RCS/arena.doc,v 1.10 1997/02/21 19:45:19 drh Exp $";
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "basictypes.h"

//#include "assert.h"
//#include "except.h"
#include "arena.h"
#define T Arena_T
//const Except_T Arena_NewFailed =
//	{ "Arena Creation Failed" };
//const Except_T Arena_Failed    =
//	{ "Arena Allocation Failed" };
#define THRESHOLD 10
struct T {
	T prev;
	char *avail;
	char *limit;
};
union align {
#ifdef MAXALIGN
	char pad[MAXALIGN];
#else
	int i;
	long l;
	long *lp;
	void *p;
	void (*fp)(void);
	float f;
	double d;
	long double ld;
#endif
};
union header {
	struct T b;
	union align a;
};
static T freechunks;
static int nfree;
T Arena_new(void) {
	T arena = malloc(sizeof (*arena));
	if (arena == NULL)
		panic("Arena_NewFailed");
	arena->prev = NULL;
	arena->limit = arena->avail = NULL;
	return arena;
}
void Arena_dispose(T *ap) {
	assert(ap && *ap);
	Arena_free(*ap);
	free(*ap);
	*ap = NULL;
}
void *Arena_alloc(T arena, long nbytes,
	const char *file, int line) {
	assert(arena);
	assert(nbytes > 0);
	nbytes = ((nbytes + sizeof (union align) - 1)/
		(sizeof (union align)))*(sizeof (union align));
	while (nbytes > arena->limit - arena->avail) {
		T ptr;
		char *limit;
		if ((ptr = freechunks) != NULL) {
			freechunks = freechunks->prev;
			nfree--;
			limit = ptr->limit;
		} else {
			long m = sizeof (union header) + nbytes + 10*1024;
			ptr = malloc(m);
			if (ptr == NULL)
				{
					if (file == NULL)
						panic("Arena_Failed");
					else
					  {
					    fprintf(stderr, "\nArena_alloc: allocation failed at %s:%d\n", file, line);
					    panic("Arena_alloc: allocation failed");
					    //Except_raise(&Arena_Failed, file, line);
					  }
				}
			limit = (char *)ptr + m;
		}
		*ptr = *arena;
		arena->avail = (char *)((union header *)ptr + 1);
		arena->limit = limit;
		arena->prev  = ptr;
	}
	arena->avail += nbytes;
	return arena->avail - nbytes;
}
void *Arena_calloc(T arena, long count, long nbytes,
	const char *file, int line) {
	void *ptr;
	assert(count > 0);
	ptr = Arena_alloc(arena, count*nbytes, file, line);
	memset(ptr, '\0', count*nbytes);
	return ptr;
}
void Arena_free(T arena) {
	assert(arena);
	while (arena->prev) {
		struct T tmp = *arena->prev;
		if (nfree < THRESHOLD) {
			arena->prev->prev = freechunks;
			freechunks = arena->prev;
			nfree++;
			freechunks->limit = arena->limit;
		} else
			free(arena->prev);
		*arena = tmp;
	}
	assert(arena->limit == NULL);
	assert(arena->avail == NULL);
}
