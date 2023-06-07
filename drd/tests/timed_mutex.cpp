#include <thread>
#include <iostream>
#include <chrono>
#include <mutex>
#include <cassert>
 
std::timed_mutex test_mutex;
int global;
 
void f()
{
    auto now=std::chrono::steady_clock::now();
    test_mutex.try_lock_until(now + std::chrono::seconds(11));
    --global;
    std::this_thread::sleep_for(std::chrono::seconds(1));
    test_mutex.unlock();
}


int main()
{
    global = 0;
    std::thread t(f);
    std::this_thread::sleep_for(std::chrono::seconds(1));
    auto now=std::chrono::steady_clock::now();
    test_mutex.try_lock_until(now + std::chrono::seconds(11));
    ++global;
    test_mutex.unlock();
    t.join();
    assert(global == 0);
}

