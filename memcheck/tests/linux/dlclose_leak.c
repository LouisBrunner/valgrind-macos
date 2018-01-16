/*  Test reporting of memory leaks in objects that have been dlopen'ed.
 *   File:   dlclose_leak.c */

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <assert.h>

int (*jmp_on_uninit)(void);
char* (*alloc_1_byte)(void);

int main(int argc, char** argv)
{
    int i; for (i = 0; i < 2; ++i)
    {
        char* memToLeak;
        char x __attribute__((unused));
        void* handle = dlopen("./dlclose_leak_so.so", RTLD_NOW);
        if(!handle) {
            printf("FAILURE to dlopen dlclose_leak_so.so\n");
            return EXIT_FAILURE;
        }
        jmp_on_uninit = dlsym(handle,"jmp_on_uninit");
        //fprintf(stderr, "jmp_on_uninit: %p\n", jmp_on_uninit);
        assert(jmp_on_uninit);
        alloc_1_byte = dlsym(handle,"alloc_1_byte");
        //fprintf(stderr, "alloc_1_byte: %p\n", alloc_1_byte);
        assert(alloc_1_byte);
        (void)jmp_on_uninit();
        memToLeak = alloc_1_byte();
        dlclose(handle);
        x = memToLeak[-1];
    }
    fprintf(stderr, "done!\n");
    return (EXIT_SUCCESS);
}
