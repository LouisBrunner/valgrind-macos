#include <stdlib.h>
#include <stdio.h>

class Test {
public:
  int a, b, c, d;
};

void *operator new[](size_t size)
{
  void *ret = malloc(size);
  printf("Here.\n");
  for (unsigned int i = 0; i < size; i++) ((char *) ret)[i] = 0xFF;
  return ret;
}

int main(int argc, char *argv[]) {
  Test *toto;
  int i;
  int j = 0;

  toto = new Test[2];

  for (i = 0; i < 2; i++) {
    if (toto[i].a) {
      j++;
    }
    //printf("%d : %08x %08x %08x %08x\n", i, toto[i].a, toto[i].b, toto[i].c, toto[i].d);
  }
}
