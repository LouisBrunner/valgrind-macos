#include <stdio.h>
#include <unistd.h>

int     main()
{
    int pgid;

    if ((pgid = getpgid(0)) == -1)
        perror("getpgid");
/*
    else
        printf("pgid : %d\n", pgid);
*/
    return pgid;
}

