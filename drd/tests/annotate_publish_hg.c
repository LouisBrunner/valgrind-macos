/* Test for verifying that unsupported annotations are reported properly. */

#include <stdio.h>
#include "../../helgrind/helgrind.h"

int main(int argc, char** argv)
{
  ANNOTATE_PUBLISH_MEMORY_RANGE(argv[0], sizeof(argv[0]));
  fprintf(stderr, "Done.\n");
  return 0;
}
