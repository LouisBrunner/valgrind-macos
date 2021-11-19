/* See also https://bugs.kde.org/show_bug.cgi?id=445504 */

#include <condition_variable>
#include <future>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

using lock_guard = std::lock_guard<std::mutex>;
using unique_lock = std::unique_lock<std::mutex>;

struct state {
  std::mutex m;
  std::vector<int> v;
  std::condition_variable cv;

  state() {
    // Call pthread_cond_init() explicitly to let DRD know about 'cv'.
    pthread_cond_init(cv.native_handle(), NULL);
  }
};

void other_thread(state *sp) {
  state &s = *sp;
  std::cerr << "Other thread: waiting for notify\n";
  unique_lock l{s.m};
  while (true) {
    if (s.cv.wait_for(l, std::chrono::seconds(3)) !=
	std::cv_status::timeout) {
      std::cerr << "Other thread: notified\n";
      break;
    }
  }
  return;
}


int main() {
  state s;
  auto future = std::async(std::launch::async, other_thread, &s);

  if (future.wait_for(std::chrono::seconds(1)) != std::future_status::timeout) {
    std::cerr << "Main: other thread returned too early!\n";
    return 2;
  }

  {
    std::lock_guard<std::mutex> g{s.m};
    s.v.push_back(1);
    s.v.push_back(2);
    s.cv.notify_all();
  }
  return 0;
}
