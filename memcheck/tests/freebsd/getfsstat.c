#include <sys/param.h>
#include <sys/ucred.h>
#include <sys/mount.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    struct statfs mbuf[128];
    int fs_count;

    if ((fs_count = getfsstat (NULL, 0, MNT_NOWAIT)) != -1 )
    {
        getfsstat (mbuf, fs_count * sizeof(mbuf[0]), MNT_NOWAIT);

        for ( int i = 0; i < fs_count; ++i)
        {
            printf("mount from name %s\n", mbuf[i].f_mntfromname);
        }
    }
 
    // now some bad uses
    struct statfs* badbuf = malloc(sizeof(struct statfs));
    free(badbuf);
    getfsstat(badbuf, 1, MNT_NOWAIT);
 
    struct statfs betterbuf;
    int i;
    int badflags;
    getfsstat(NULL, i, MNT_NOWAIT);
    getfsstat(&betterbuf, sizeof(struct statfs), badflags);
}

