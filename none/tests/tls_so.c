#include <config.h>

#ifdef HAVE_TLS
#include <pthread.h>

extern __thread int so_extern;
static __thread int so_local;
extern __thread int global;

int *test_so_extern(void)
{
	return &so_extern;
}

int *test_so_local(void)
{
	return &so_local;
}

int *test_so_global(void)
{
	return &global;
} 
#endif
