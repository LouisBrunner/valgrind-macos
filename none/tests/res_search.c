#include <stdio.h>
#include <string.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <pthread.h>

void* fn(void* arg)
{
    char* dn = (char*)arg;

    unsigned char buff[8000];

    if(-1 == res_search(dn, 1, 1, buff, 8000))
    {
        printf("Error: res_search()\n");
    }
    else
    {
        printf("Success!\n");
    }
    return 0;
}

int main(int argc, char** argv)
{
    pthread_t pid;
    if(2 != argc)
    {
        printf("Usage: %s <domain>\n", argv[0]);
        return 1;
    }

    _res.options |= RES_DEBUG;
    if(0 != res_init())
    {
        printf("Error: res_init()\n");
        return(1);
    }
#if 1
    /* Test it in a different thread -- the failure case */
    if(0 != pthread_create(&pid, 0, fn, (void*)argv[1]))
    {
        printf("Failed to create thread.\n");
        return 1;
    }

    pthread_join(pid, 0);
#else
    {
    unsigned char buff[8000];

    if(-1 == res_search(argv[1], 1, 1, buff, 8000))
    {
        printf("Error: res_search()\n");
    }
    else
    {
        printf("Success!\n");
    }
    }
#endif
    return 0;
}


