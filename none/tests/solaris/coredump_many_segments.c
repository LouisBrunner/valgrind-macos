/* Tests that Valgrind coredump support works correctly even when
   number of segments exceeds 0xffff.
   For this to work, large number of pages is mmap()'ed into the
   process (virtual) address space. These pages must not be adjacent
   to each other otherwise the memory manager will coalesce them
   into a single one. So they are one page apart.

   NOTE: Valgrind's internal limit VG_N_SEGMENTS must be at least
   140000 otherwise you get a fatal error similar to this one:
       "FATAL: VG_N_SEGMENTS is too low."

   Test case passes successfully if the number of segments is
   correctly displayed in elfdump output:

   $ elfdump -e vgcore.*
ELF Header
  ...
  e_phoff: 0x34  e_phentsize: 32  e_phnum: PN_XNUM (see shdr[0].sh_info)
                                  ^^^^^^^^^^^^^^^^
Section Header[0]:  (ELF Ehdr extensions)
  ...
    sh_link: 0 (e_shstrndx)  sh_info: 65554 (e_phnum)
                             ^^^^^^^^^^^^^^^^^^^^^^^^
*/ 

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/mman.h>
#include <sys/procfs.h>
#include <sys/stat.h>

#define SEGMENTS (0xffff + 2)

#if 0
#define DEBUG(format, ...) printf(format, ## __VA_ARGS__)
#else
#define DEBUG(format, ...)
#endif

#define PRINT(format, ...) printf(format, ## __VA_ARGS__)

/* Represents a free range of a virtual address space. */
typedef struct range {
   uintptr_t     start;
   uintptr_t     end;
   size_t        size;
   struct range *next;
} range_t;

/* Processes a single prmap_t entry and builds the free ranges. */
static int process_map(const prmap_t *map, range_t **ranges_head,
                       range_t **ranges_tail, size_t page_size)
{
   assert(map != NULL);
   assert(ranges_head != NULL);
   assert(ranges_tail != NULL);

   range_t *head = *ranges_head;
   range_t *tail = *ranges_tail;

   DEBUG("processing map with addr=%p and size=%zu\n",
         map->pr_vaddr, map->pr_size);

   if (head == NULL) {
      head = calloc(1, sizeof(range_t));
      if (head == NULL) {
         fprintf(stderr, "calloc failed\n");
         return -1;
      }
      head->start = (uintptr_t) page_size; // do not start at address '0'

      tail = head;
      *ranges_head = head;
      *ranges_tail = tail;
   }

   if ((map->pr_vaddr < tail->start) ||
       (map->pr_vaddr - tail->start < 3 * page_size)) {
      DEBUG("last range at %p is too small, skipping it\n",
            tail->start);
      tail->start = map->pr_vaddr + map->pr_size + page_size;
      return 0;
   }

   tail->end = map->pr_vaddr - page_size;
   tail->size = tail->end - tail->start;

   range_t *new_one = calloc(1, sizeof(range_t));
   if (new_one == NULL) {
      fprintf(stderr, "calloc failed\n");
      return -1;
   }

   new_one->start = map->pr_vaddr + map->pr_size + page_size;
   tail->next = new_one;
   *ranges_tail = new_one;
   return 0;
}

/* Reads /proc/self/map and builds free ranges. */
static range_t *read_proc_map(size_t page_size)
{
   int fd = open("/proc/self/map", O_RDONLY);
   if (fd == -1) {
      int error = errno;
      fprintf(stderr, "open failed: %s (%d)\n", strerror(error), error);
      return NULL;
   }

   prmap_t map;
   range_t *ranges_head = NULL;
   range_t *ranges_tail = NULL;

   ssize_t bytes = read(fd, &map, sizeof(map));
   while (bytes == sizeof(map)) {
      if (map.pr_size != 0) {
         if (process_map(&map, &ranges_head, &ranges_tail,
                         page_size) != 0) {
            return NULL;
         }
      }
      bytes = read(fd, &map, sizeof(map));
   }

   if (ranges_tail != NULL) {
      ranges_tail->end = (uintptr_t) ~0;
      ranges_tail->size = ranges_tail->end - ranges_tail->start;
   }

   close(fd);
   return ranges_head;
}

static void print_ranges(const range_t *head)
{
   while (head != NULL) {
      DEBUG("free range [%8p - %8p] of size %7zuK\n",
            head->start, head->end, head->size / 1024);
      head = head->next;
   }
}

static size_t sum_ranges(const range_t *head)
{
   size_t sum = 0;

   while (head != NULL) {
      sum += head->size;
      head = head->next;
   }

   return sum;
}

static void *map_segment(void *fixed_addr)
{
   int flags = MAP_NORESERVE | MAP_ANON | MAP_PRIVATE | MAP_FIXED;
   void *addr = mmap(fixed_addr, 1, PROT_READ | PROT_WRITE,
                     flags, -1, 0);
   if (addr == MAP_FAILED) {
      int error = errno;
      fprintf(stderr, "mmap failed: %s (%d)\n", strerror(error), error);
      return NULL;
   }
   assert(addr == fixed_addr);

   *((char *) addr) = 1; // make the mmap'ed page dirty
   // DEBUG("mmap(%8p) = %8p\n", fixed_addr, addr);
   return addr;
}

int main(int argc, const char *argv[])
{
   long page_size = sysconf(_SC_PAGESIZE);
   if (page_size == -1) {
      perror("sysconf");
      return 1;
   }

   PRINT("Page size determined as %ld bytes.\n", page_size);

   range_t *ranges = read_proc_map(page_size);
   print_ranges(ranges);

   size_t sum = sum_ranges(ranges);
   if (sum < SEGMENTS * page_size) {
      fprintf(stderr, "Free (virtual) address space cannot accomodate "
              "%u pages.\n", SEGMENTS);
      return 1;
   }

   PRINT("mmap()'ing %u segments:", SEGMENTS);
   fflush(stdout);

   unsigned int segment = 0;
   while ((ranges != NULL) && (segment < SEGMENTS)) {
      unsigned int page;
      for (page = 0; page < ranges->size / (2 * page_size); page++) {
         uintptr_t start = ranges->start + 2 * page * page_size;
         void *addr = map_segment((void *) start);
         if (addr == NULL) {
            fprintf(stderr, "Mapping failed for segment %u at address "
                    "%" PRIxPTR ".\n", segment, start);
            return 1;
         }

         segment += 1;
         if (segment >= SEGMENTS) {
            break;
         }

         if (segment % (SEGMENTS / 10) == 0) {
            PRINT(" %u0%%", segment / (SEGMENTS / 10));
            fflush(stdout);
         }
      }
      ranges = ranges->next;
   }
   assert(segment == SEGMENTS);

   PRINT(".\nDumping core...\n");
   char *nihil = NULL;
   *nihil = 0; // SEGV here
   fprintf(stderr, "Should not reach here.\n");

   return 0;
}
