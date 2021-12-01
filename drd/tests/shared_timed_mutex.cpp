#include <thread>
#include <iostream>
#include <chrono>
#include <shared_mutex>
#include <mutex>
#include <cassert>
#include <condition_variable>

std::shared_timed_mutex test_mutex;
std::mutex cv_mutex;
std::condition_variable cv;
int global;
bool reads_done = false;
 
void f()
{
    auto now=std::chrono::steady_clock::now();
    if (test_mutex.try_lock_until(now + std::chrono::seconds(3)))
    {
       --global;
       test_mutex.unlock();
    }
    else
    {
        std::cerr << "Lock failed\n";
    }
}

void g()
{
    auto now=std::chrono::steady_clock::now();
    if (test_mutex.try_lock_shared_until(now + std::chrono::seconds(2)))
    {
       test_mutex.unlock_shared();
    }
    else
    {
        std::cerr << "Lock shared failed\n";
    }
    std::unique_lock<std::mutex> lock(cv_mutex);
    reads_done = true;
    cv.notify_all();
}
 
int main()
{
    global = 1;
    test_mutex.lock_shared();
    std::thread t1(f);
    std::thread t2(g);
    {
       std::unique_lock<std::mutex> lock(cv_mutex);
       while (!reads_done)
       {
          cv.wait(lock);
       }
    }
    test_mutex.unlock_shared();
    t1.join();
    t2.join();
    assert(global == 0);
}

