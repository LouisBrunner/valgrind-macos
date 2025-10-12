/* Regression test for bug #331311: Valgrind file descriptors visible in /proc/self/fd and /proc/self/fdinfo */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>

struct linux_dirent64 {
    ino64_t  d_ino;
    off64_t  d_off;
    unsigned short d_reclen;
    unsigned char  d_type;
    char           d_name[];
};

/* Small buffer size to test retry logic when all entries get filtered */
#define SMALL_BUF_SIZE 64

/* Helper function to scan a /proc directory and print numeric FD entries with a prefix.
   Returns 0 on success, -1 if directory unavailable */
static int scan_proc_fd_directory(const char *dir_path, const char *prefix)
{
    DIR *d = opendir(dir_path);
    if (d == NULL) {
        printf("%s not available\n", dir_path);
        return -1;
    }

    struct dirent *entry;
    while ((entry = readdir(d)) != NULL) {
        if (entry->d_name[0] == '.')
            continue;

        char *endptr;
        long fd = strtol(entry->d_name, &endptr, 10);
        if (*endptr == '\0' && fd >= 0) {
            if (prefix) {
                printf("%s:%ld\n", prefix, fd);
            } else {
                printf("%ld\n", fd);
            }
        }
    }
    closedir(d);
    return 0;
}

/*
 * Test retry logic with raw getdents syscall and small buffer.
 *
 * This test validates the filtering refill mechanism in syswrap-generic.c.
 * When using a tiny buffer (64 bytes), getdents may return only Valgrind FDs
 * in a single call. The filtering code should detect this (new_size == 0) and
 * automatically retry the syscall to get more entries until it finds client FDs
 * or reaches EOF. This prevents the client from seeing empty results when
 * Valgrind FDs get filtered out.
 */
static void test_retry_logic_with_small_buffer(void)
{
    int fd;
    char buf[SMALL_BUF_SIZE];
    long nread;
    struct linux_dirent64 *d;

    printf("retry_test_start\n");

    fd = open("/proc/self/fd", O_RDONLY | O_DIRECTORY);
    if (fd == -1) {
        printf("retry_test_unavailable\n");
        return;
    }

    /*
     * Use raw getdents syscall with tiny 64-byte buffer. This forces multiple
     * syscalls since each directory entry is typically 20+ bytes. Some calls
     * may return only Valgrind FDs, which will trigger the retry mechanism.
     */
    for (;;) {
        /* Note, using getdents64 since some linux arches don't implement
           the 32bit getdents. */
        nread = syscall(SYS_getdents64, fd, buf, SMALL_BUF_SIZE);

        if (nread == -1) {
            printf("retry_test_error\n");
            close(fd);
            return;
        }

        if (nread == 0) {
            break; /* EOF - no more entries */
        }

        /* Print client FD entries found in this buffer (excluding . and ..) */
        for (size_t bpos = 0; bpos < nread;) {
            d = (struct linux_dirent64 *)(buf + bpos);
            if (strcmp(d->d_name, ".") != 0 && strcmp(d->d_name, "..") != 0) {
                char *endptr;
                long fd_num = strtol(d->d_name, &endptr, 10);
                if (*endptr == '\0' && fd_num >= 0) {
                    printf("retry:%ld\n", fd_num);
                }
            }
            bpos += d->d_reclen;
        }
    }

    close(fd);
    printf("retry_test_end\n");
}

int main(void)
{
    /* Test 0: Retry logic with small buffer (tests the filtering refill mechanism) */
    test_retry_logic_with_small_buffer();

    /* Test 1: /proc/self/fd should have Valgrind FDs filtered out */
    if (scan_proc_fd_directory("/proc/self/fd", NULL) == -1) {
        return 0; /* Skip remaining tests if /proc/self/fd unavailable */
    }

    /* Test 2: /proc/self/fdinfo should have Valgrind FDs filtered out */
    scan_proc_fd_directory("/proc/self/fdinfo", "fdinfo");

    /* Test 3: Regular directory should show all numbered files */
    DIR *d;
    struct dirent *entry;
    if (mkdir("testdir", 0755) == 0) {
        FILE *f1 = fopen("testdir/1000", "w");
        FILE *f2 = fopen("testdir/1001", "w");
        if (f1) { fprintf(f1, "test"); fclose(f1); }
        if (f2) { fprintf(f2, "test"); fclose(f2); }
        
        d = opendir("testdir");
        if (d != NULL) {
            while ((entry = readdir(d)) != NULL) {
                if (entry->d_name[0] != '.') {
                    char *endptr;
                    long num = strtol(entry->d_name, &endptr, 10);
                    if (*endptr == '\0' && num >= 1000) {
                        printf("regular:%ld\n", num);
                    }
                }
            }
            closedir(d);
        }
        
        unlink("testdir/1000");
        unlink("testdir/1001");
        rmdir("testdir");
    }
    
    
    return 0;
}
