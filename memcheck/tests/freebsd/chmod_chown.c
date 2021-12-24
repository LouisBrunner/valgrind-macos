/*
 * Test this family of functions
 * lchmod chownat lchownat
 */

#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int main(void)
{
    char buff[64];
    char tmpfile[] = "/tmp/test_chmod_chown.XXXXXX";
    char tmplink[] = "/tmp/test_chx_link.XXXXXX";
    int tmpfd = mkstemp(tmpfile);
    mktemp(tmplink);

    memset(buff, 0, sizeof(buff));
    sprintf(buff, "some data");
    write(tmpfd, buff, strlen(buff)+1);
    close (tmpfd);

    DIR* tmpdir = opendir("/tmp");
    if (tmpdir) {
       int tmpdirfd = dirfd(tmpdir);
 
       if (-1 == symlinkat(tmpfile+5, tmpdirfd, tmplink+5)) {
           perror("linkat failed");
       }
 
       if (-1 == lchmod(tmplink, S_IRWXU|S_IRWXG|S_IRWXO))
       {
          perror("lchmod failed:");
       }
 
       if (fchmodat(tmpdirfd, tmpfile+5, S_IRWXU|S_IRWXG|S_IRWXO, 0))
       {
          perror("fchmodat failed:");
       }
 
       // no test for failure as not everyone runnning this will be a member of group 921
       fchownat(tmpdirfd, tmpfile+5, getuid(), 920, 0);

       closedir(tmpdir);
    }

    unlink(tmpfile);
    unlink(tmplink);
 
    // error section
    char* badstring = strdup("foo");
    free(badstring);
    int badint1;
    int badint2;
    int badint3;
    int badint4;
 
    lchmod(badstring, badint1);
    fchmodat(badint1, badstring, badint2, badint3);
    fchownat(badint1, badstring, badint2, badint3, badint4);
}

