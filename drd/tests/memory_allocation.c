/**
 * @brief Repeatedly allocate and free memory. Tests whether drd really frees
 *        memory allocated by a client. See also
 *        http://bugs.kde.org/show_bug.cgi?id=161036.
 */

#include <assert.h>
#include <stdlib.h>

int main()
{
  int i;
  void* p;

  for (i = 0; i < 100000; i++)
    free(malloc(40960));

  for (i = 0; i < 100000; i++)
  {
    p = realloc(NULL, 40960);
    p = realloc(p, 50000);
    p = realloc(p, 40000);
    p = realloc(p, 0);
    /*
     * glibc returns a NULL pointer when the size argument passed to realloc()
     * is zero, while Darwin's C library returns a non-NULL pointer. Both are
     * allowed by POSIX.
     *
     * Other platforms also tend not to free. To make things simpler just
     * free it if it is not NULL.
     */
    if (p)
      free(p);
  }

  return 0;
}
