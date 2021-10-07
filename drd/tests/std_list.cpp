/*
 * Test program that triggers strcpy() from one thread and a memory allocation
 * immediately after the region read by strcpy() from another thread. Without
 * strcpy() intercept there is about 50% chance that this test program triggers
 * a false positive race report on Ubuntu 12.10 amd64.
 *
 * See also https://bugs.kde.org/show_bug.cgi?id=326436.
 */

#include <locale.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <string>
#include <sstream>
#include <list>
#if defined(__FreeBSD__)
#include <mutex>
#endif

using namespace std;

#if defined(__FreeBSD__)
std::mutex g_mutex;

// according to this
// https://stackoverflow.com/questions/4057319/is-setlocale-thread-safe-function
// setlocale is not thread safe, and indeed on FreeBSD
// a load of errors are generated if this is not guarded
void setlocale_wrapper()
{
   const std::lock_guard<std::mutex> lock(g_mutex);
   setlocale(LC_ALL, "English");
}

#else

void setlocale_wrapper()
{
   setlocale(LC_ALL, "English");
}

#endif

class SubTest {
public:
  SubTest() {
    list<int *> ffList;
    ffList.push_back((int *) NULL);
    for (list<int*>::iterator ff = ffList.begin(); ff != ffList.end(); ff++) {
      usleep(1000);
    }
  }
  void subTest() {
    list<int *> ffList;
    ffList.push_back((int *) NULL);
    for (list<int*>::const_iterator ff = ffList.begin(); ff != ffList.end(); ff++) {
      usleep(1000);
    }
  }
};

class Test {
  SubTest *subTest;
public:
  void setUp() {
    subTest = new SubTest();
    setlocale_wrapper();
  }
  void tearDown() {
    delete subTest; }
  void func1() {
    for (size_t i = 0; i < 10000; i++) {
      subTest->subTest();
      usleep(1000);
    }
  }
  void func2() {
    usleep(1000);
  }
};

void *func1(void *instance)
{
  Test *casted = reinterpret_cast<Test*>(instance);
  casted->setUp();
  casted->func1();
  casted->tearDown();
  return NULL;
}

void *func2(void *instance)
{
  Test *casted = reinterpret_cast<Test*>(instance);
  casted->setUp();
  casted->func2();
  casted->tearDown();
  return NULL;
}

int main(int argc, char* argv[])
{
  int err;
  pthread_t thread1;
  pthread_t thread2;
  Test instance1;
  Test instance2;

  // create
  err = pthread_create(&thread1, NULL, &func1, &instance1);
  if (err != 0)
    throw string("failed to create a thread.");
  err = pthread_create(&thread2, NULL, &func2, &instance2);
  if (err != 0)
    throw string("failed to create a thread.");
  // join
  err = pthread_join(thread1, NULL);
  if (err != 0)
    throw string("Thread::join(): failed to join.");
  err = pthread_join(thread2, NULL);
  if (err != 0)
    throw string("Thread::join(): failed to join.");
}
