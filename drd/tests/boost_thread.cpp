// Test program that allows to verify whether Drd works fine for programs that
// use the boost::thread, boost::mutex and boost::condition classes.


#include <boost/thread/condition.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/thread.hpp>
#include <iostream>


static boost::condition s_cva;
static boost::mutex s_m;


static void thread_func(void)
{
  std::cerr << "Thread 2.\n";
  boost::mutex::scoped_lock sl(s_m);
  s_cva.notify_all();
  s_cva.wait(sl);
}

int main(int argc, char** argv)
{
  std::cerr << "Thread 1.\n";
  boost::mutex::scoped_lock sl(s_m);
  boost::thread t(thread_func);
  s_cva.wait(sl);
  s_cva.notify_all();
  sl.unlock();
  t.join();
  std::cerr << "Finished.\n";
  return 0;
}
