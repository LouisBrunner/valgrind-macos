#include <iostream>
#include <cerrno>
#include <cstring>
#include <pthread.h>
#include <semaphore.h>

struct FailedOn
{
  FailedOn(const char *f, int e) : func(f), eno(e) {}
  std::string func;
  int eno;
};

int main(void)
{
  try {
    sem_t mySemaphore;
    if (sem_init(&mySemaphore, 0, 0) != 0)
      throw FailedOn("sem_init", errno);

    std::cerr << "Calling sem_trywait\n";
    if (sem_trywait(&mySemaphore) != 0) {
      if (errno != EAGAIN)
        throw FailedOn("sem_trywait", errno);
		
      std::cerr << "Wait would have blocked" << std::endl;
    } else {
      std::cerr << "Wait succeeded" << std::endl;
    }

    if (sem_destroy(&mySemaphore) != 0)
      throw FailedOn("sem_destroy", errno);

    return 0;
  } catch (FailedOn &e) {
    std::cerr << e.func << " failed: " << strerror(e.eno) << std::endl;
  } catch (...) {
    std::cerr << "Unknown exception" << std::endl;
  }
	
  return -1;
}
