#define _GNU_SOURCE

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

#include "../../memcheck.h"

static void
assert_expected (int fd, int expected_seals)
{
  int current_seals = fcntl (fd, F_GET_SEALS);
  assert (current_seals == expected_seals);
}

static void
add_seal (int fd, int *expected_seals, int new_seal)
{
  int r = fcntl (fd, F_ADD_SEALS, new_seal);
  assert (r == 0);

  *expected_seals |= new_seal;

  // Make sure we get the result we expected.
  assert_expected (fd, *expected_seals);
}

static void
add_seal_expect_fail (int fd, int new_seal, int expected_errno)
{
  int r = fcntl (fd, F_ADD_SEALS, new_seal);
  assert (r == -1 && errno == expected_errno);
}

int
main (void)
{
  int expected_seals = 0;
  int fd;

  // Try with an fd that doesn't support sealing.
  fd = memfd_create ("xyz", 0);
  if (fd < 0)
    {
      // Not supported, nothing to test...
      return 1;
    }

  assert_expected (fd, F_SEAL_SEAL);
  add_seal_expect_fail (fd, F_SEAL_WRITE, EPERM);
  assert_expected (fd, F_SEAL_SEAL); // ...should still be unset after failed attempt
  close (fd);

  // Now, try the successful case.
  fd = memfd_create ("xyz", MFD_ALLOW_SEALING);
  add_seal (fd, &expected_seals, F_SEAL_SHRINK);
  add_seal (fd, &expected_seals, F_SEAL_GROW);

  // Now prevent more sealing.
  add_seal (fd, &expected_seals, F_SEAL_SEAL);

  // And make sure that it indeed fails.
  add_seal_expect_fail (fd, F_SEAL_WRITE, EPERM);
  assert_expected (fd, expected_seals);
  close (fd);

  // Test the only memory failure possible: passing an undefined argument to F_ADD_SEALS
  int undefined_seal = 0;
  VALGRIND_MAKE_MEM_UNDEFINED(&undefined_seal, sizeof undefined_seal);
  fcntl (-1, F_ADD_SEALS, undefined_seal);

  return 0;
}
