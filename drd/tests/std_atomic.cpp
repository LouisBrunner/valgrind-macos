/*
 * Test program for std::atomic<>
 *
 * See also https://bugs.kde.org/show_bug.cgi?id=328490.
 */

#include "../drd.h"
#include <atomic>
#include <iostream>
#include <string>
#include <pthread.h>

std::atomic<bool> g_b;

void *func1(void *instance)
{
  while (!g_b) {
    timespec delay = { 0, 100 * 1000 * 1000 };
    nanosleep(&delay, NULL);
  }
  return NULL;
}

void *func2(void *instance)
{
  g_b = true;
  return NULL;
}

int main(int argc, char* argv[])
{
  int err;
  pthread_t thread1;
  pthread_t thread2;

  std::cerr << "Started.\n";

  if (argc > 1)
    DRD_IGNORE_VAR(g_b);

  err = pthread_create(&thread1, NULL, &func1, NULL);
  if (err != 0)
    throw std::string("failed to create a thread.");
  err = pthread_create(&thread2, NULL, &func2, NULL);
  if (err != 0)
    throw std::string("failed to create a thread.");

  err = pthread_join(thread1, NULL);
  if (err != 0)
    throw std::string("Thread::join(): failed to join.");
  err = pthread_join(thread2, NULL);
  if (err != 0)
    throw std::string("Thread::join(): failed to join.");

  std::cerr << "Done.\n";
}
