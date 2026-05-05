#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/fs.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char** argv)
{
	char name[256];
        char buildid[256];
        char cwd[256];
        getcwd(cwd, sizeof(cwd));

	struct procmap_query pq = {
		.size = sizeof(pq), .query_addr = (uintptr_t)main,
		.vma_name_size = 256, .vma_name_addr = (uintptr_t)name,
                .build_id_size = 256, .build_id_addr = (uintptr_t)buildid
	};
	int fd = open("/proc/self/maps", O_RDONLY);
	ioctl(fd, PROCMAP_QUERY, &pq);
        // print name but strip off the PWD prefix so that
        // we always get a known output we can easily check
        puts(name + strlen(cwd) + 1);
        // buildid is a binary blob, not NUL terminated C string
        buildid[pq.build_id_size-1] = '\0';
        // make sure that kernel returned some reasonable build_id_size
        if (pq.build_id_size > 0 && pq.build_id_size < 256)
            puts("OK");
        // print the buildid so that the bug can trigger
        puts(buildid);
}

