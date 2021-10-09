#include <sys/param.h>
#include <sys/mount.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <err.h>
#include <unistd.h>
#include "../../../config.h"

int main()
{
   char path[PATH_MAX];
   fhandle_t fh1;
   fhandle_t fh2;
#if (FREEBSD_VERS >= FREEBSD_12)
   fhandle_t fh3;
#endif
 
   int fd;
 
   strcpy(path, "/tmp/getfh.XXXXXXXXXXX");
   fd = mkstemp(path);
   if (fd < 0)
      err(-1, "mkstemp(%s)", path);
 
    getfh(path, &fh1);
    lgetfh(path, &fh2);
#if (FREEBSD_VERS >= FREEBSD_12)    
    getfhat(fd, path, &fh3, AT_SYMLINK_NOFOLLOW);
#endif
 
    // error section
    char* fn = strdup(path);
    fhandle_t* pfh1 = malloc(sizeof(fhandle_t));
    int* pi = malloc(sizeof(int));
    free(fn);
    free(pfh1);
 
    getfh(fn, pfh1);
    lgetfh(fn, pfh1);
#if (FREEBSD_VERS >= FREEBSD_12)    
    getfhat(*pi+AT_FDCWD, fn, pfh1, *pi+AT_SYMLINK_NOFOLLOW);
#endif
 
    free(pi);
    close(fd);
    unlink(path);
}

