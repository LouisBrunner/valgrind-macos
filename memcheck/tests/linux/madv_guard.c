#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int fd = -1; // for /proc/self/pagemap
size_t ps = 0; // page size

void check_addr(char *p)
{
    // check page aligned-ness
    if ((uintptr_t)p % ps != 0)
        perror("page not aligned!\n");

    // https://docs.kernel.org/admin-guide/mm/pagemap.html
    off_t offset = ((uintptr_t)p / ps) * sizeof(uint64_t);
    uint64_t entry; // one 64-bit value for each virtual page
    if (lseek(fd, offset, SEEK_SET) == (off_t)-1)
        perror("lseek /proc/self/pagemap failed\n");

    read(fd, &entry, sizeof(entry));
    // Bit 58 pte is a guard region (since 6.15) (see madvise (2) man page)
    printf("page 0x%lx, is guardpage: %lu, present: %lu\n",
           (unsigned long)p, ((entry >> 58) & 1), ((entry >> 63) & 1));

    // attempt accessing the address
    // if ((open(p, O_RDONLY) == -1) && errno == EFAULT)
    //     // EFAULT means that we've hit guarded region
    //     printf("accessing address 0x%lx failed\n", (unsigned long)p);
    // else if ((open(p, O_RDONLY) == -1) && errno == ENOENT)
    //     // ENOENT means no such file or directory, and that's OK here
    //     // we didn't hit a guarded region, we consider this a pass
    //     printf("accessing address 0x%lx passed\n", (unsigned long)p);
    // else
    //     // We never open a valid file here, so this can't happen!
    //     printf("the impossible happened, open() succeeded!\n");

}

void install_guardpage(char *p)
{
    printf("Installing guard page, addr=0x%lx\n", (unsigned long)p);
    int rv = madvise(p, ps, MADV_GUARD_INSTALL);
    if (rv != 0)
        perror("madvise failed\n");
}

void remove_guardpage(char *p)
{
    printf("Removing guard page, addr=0x%lx\n", (unsigned long)p);
    int rv = madvise(p, ps, MADV_GUARD_REMOVE);
    if (rv != 0)
        perror("madvise failed\n");
}

int main(void)
{
    fd = open("/proc/self/pagemap", O_RDONLY);
    if (fd < 0)
        perror("open /proc/self/pagemap failed\n");

    ps = sysconf(_SC_PAGESIZE);

    char *p = mmap(NULL, ps, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    printf("force page-in by printing this: %d\n", p[0]);

    check_addr(p);
    install_guardpage(p);
    check_addr(p);
    remove_guardpage(p);
    // The page-in below isn't needed from the persp of checking whether we've
    // hit a guard page or not.  It affects whether the page is present though,
    // so it does impact the expected stdout.log so to speak.
    printf("force page-in by printing this: %d\n", p[0]);
    check_addr(p);
    install_guardpage(p);
    check_addr(p);
    // A valid way to nuke a guard page is to unmap it (per man 2 madvise)
    munmap(p, ps);
    check_addr(p);

    close(fd);
    return 0;
}
