#include <semaphore.h>
#include <stdlib.h>

#define SEM_LIMIT 100

int main(int argc, char **argv)
{
  sem_t s[SEM_LIMIT];
  int i;
  
  for (i = 0; i < SEM_LIMIT; i++) {
    sem_init(&s[i], 0, 0);
  }

  exit(0);
}
