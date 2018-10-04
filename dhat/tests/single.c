// A single allocation (so the root node is the only node in the tree).

#include <stdlib.h>

int main() {
   int* a = (int*)malloc(16);
   a[0] = 0;
   a[0] = 1;
   a[0] = 2;
   return 0;
}
