/*
 * Test program that uses std::string object from more than one thread and
 * that also triggers a call to __GI_strlen() (from inside strdup()). See also
 * https://bugs.kde.org/show_bug.cgi?id=326091.
 */

#include <list>
#include <string>
#include <cstring>
#include <pthread.h>
#if defined(__FreeBSD__)
#include <stdio.h>
#endif
#include <stdlib.h>
#include <unistd.h>

char* list2byteArray()
{
  size_t data_size = 24;
  char *data = new char[data_size];
  for (size_t i = 0; i < data_size; i++)
    data[i] = 'a';
  data[data_size - 1] = 0;
  char *ret = strdup(data);
  delete[] data;
  return ret;
} 

int addRecord()
{
  char *data = list2byteArray();
  usleep(100);
  free(data);
  return 0;
}

void *fillTable(void *ptr)
{
  for (int i = 0; i < 100; i++) {
    std::string id("000");
    id.append(1, 'a' + i);
    std::list<std::string> record;
    record.push_back("some data");
    addRecord();
  }
  usleep(1000 * 1000);
  return NULL;
}

int main(int argc, char* argv[])
{
  pthread_t thread[2];

  for (int i = 0; i < sizeof(thread)/sizeof(thread[0]); i++) {
    int ret = pthread_create(&thread[i], NULL, &fillTable, NULL);
    if (ret) {
      fprintf(stderr, "Failed to create thread %d: %d\n", i, ret);
      return 1;
    }
  }

  for (int i = 0; i < sizeof(thread)/sizeof(thread[0]); i++) {
    int ret = pthread_join(thread[i], NULL);
    if (ret != 0) {
      fprintf(stderr, "Failed to join thread %d: %d\n", i, ret);
      return 1;
    }
  }

  return 0;
}
