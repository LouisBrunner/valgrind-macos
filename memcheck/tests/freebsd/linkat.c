/*
 * Test this family of functions
 * linkat, unlinkat, symlinkat
 *
 * and whilst we have an open filehandle, gratuitously test
 * fdatasync
 */

#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../../config.h"

int main(void)
{
    char buff[64];
    char tmpfromfile[] = "/tmp/testlinkat1.XXXXXX";
    char tmptolink[] = "/tmp/testlinkat2.XXXXXX";
    char tmptosymlink[] = "/tmp/testlinkat3.XXXXXX";
    int tmpfd = mkstemp(tmpfromfile);
    mktemp(tmptolink);

    memset(buff, 0, sizeof(buff));
    sprintf(buff, "some data");
    write(tmpfd, buff, strlen(buff)+1);
#if (FREEBSD_VERS >= FREEBSD_11)
    fdatasync(tmpfd);
#endif
    close (tmpfd);

    DIR* tmpdir = opendir("/tmp");
    if (tmpdir) {
        int tmpdirfd = dirfd(tmpdir);

        const char* from = tmpfromfile+5;
        const char* to = tmptolink+5;
        const char* tosym = tmptosymlink+5;
 
        if (-1 == linkat(tmpdirfd, from, tmpdirfd, to, 0)) {
            perror("linkat failed");
        }

        unlinkat(tmpdirfd, to, 0);

        if (-1 == symlinkat(from, tmpdirfd, tosym)) {
            perror("symlinkat failed");
        }

        unlinkat(tmpdirfd, tosym, 0);
 
        // now some errors
        char* badstring = strdup(from);
        free(badstring);
        linkat(tmpdirfd, badstring, tmpdirfd, to, 0);
        symlinkat(badstring, tmpdirfd, tosym);
        unlinkat(tmpdirfd, to, 0);
        unlinkat(tmpdirfd, tosym, 0);
 
        badstring = strdup(to);
        free(badstring);
        linkat(tmpdirfd, from, tmpdirfd, badstring, 0);
        unlinkat(tmpdirfd, badstring, 0);

        badstring = strdup(tosym);
        free(badstring);
        symlinkat(from, tmpdirfd, badstring);
        unlinkat(tmpdirfd, badstring, 0);

        int uninit;
        linkat(uninit, from, uninit, to, uninit);
        symlinkat(from, uninit, tosym);
        unlinkat(uninit, "dontcare", uninit);

        closedir(tmpdir);
    }
 
    unlink(tmpfromfile);
    int badint;
#if (FREEBSD_VERS >= FREEBSD_11)
    fdatasync(badint);
#endif
}

