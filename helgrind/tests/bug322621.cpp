// See also https://bugs.kde.org/show_bug.cgi?id=322621

#include <pthread.h>
#include <iostream>
#include "../../helgrind/helgrind.h"

static int verbose;
static pthread_t ls_thread;
static pthread_barrier_t ls_barrier;

char* ls_buf[2];

#define LS_BUF_SIZE (1024*1024)
#define NR_RUNS 2

void fill_buffer(int buf)
{
   if (verbose)
      std::cerr << "Fill " << buf << "\n";
   for (int i = 0; i < LS_BUF_SIZE; i++)
      ls_buf[buf][i] = 1;
   if (verbose)
      std::cerr << "Fill " << buf << " done\n";
}

int read_buffer(int buf)
{
   if (verbose)
      std::cerr << "Read " << buf << "\n";
   int res = 0;
   for (int i = 0; i < LS_BUF_SIZE; i++)
      res += ls_buf[buf][i];
   if (verbose)
      std::cerr << "Read " << buf << " done\n";
   return res;
}

void *the_thread(void *ptr)
{
   int buf = 1;

   for (int i = 0; i < NR_RUNS; i++) {

      fill_buffer(buf);

      if (verbose)
	 std::cerr << "Aux at barrier " << i << "\n";
      pthread_barrier_wait(&ls_barrier);
      if (verbose)
	 std::cerr << "Aux after barrier " << i << "\n";

      buf = buf ^ 1;
   }
   return ptr;
}


int main()
{
   VALGRIND_HG_DISABLE_CHECKING(&std::cerr, sizeof(std::cerr));
	
   ls_buf[0] = new char[LS_BUF_SIZE];
   ls_buf[1] = new char[LS_BUF_SIZE]; // second buffer only when multithreaded

   pthread_barrier_init(&ls_barrier, NULL, 2);

   pthread_attr_t attr;
   pthread_attr_init(&attr);
   pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
   pthread_create(&ls_thread, &attr, the_thread, NULL);
   pthread_attr_destroy(&attr);

   int buf = 0;
   for (int i = 0; i < NR_RUNS; i++) {
      std::cerr << "Main at barrier " << i << "\n";
      pthread_barrier_wait(&ls_barrier);
      std::cerr << "Main after barrier " << i << "\n";
      buf = buf ^ 1;
      (void)read_buffer(buf);
   }

   pthread_join(ls_thread,NULL);
   pthread_barrier_destroy(&ls_barrier);
   delete[] ls_buf[1]; // second buffer only when multithreaded
   delete[] ls_buf[0];

   return 0;
}
