// See also https://bugs.kde.org/show_bug.cgi?id=416286

#include <mutex>
#include <iostream>
#include <thread>
#include <vector>

class counter {
public:
  counter(): mutex() {}
  void get() { std::unique_lock<std::mutex> lock(mutex); }

private:
  std::mutex mutex;
};

static counter& get_counter()
{
  static counter manager;
  return manager;
}

static void do_work()
{
  get_counter().get();
}

int main()
{
  std::vector<std::thread> v;

  for (int i = 0; i < 16; i++)
    v.emplace_back([]{ do_work(); });

  for (auto& t : v)
    t.join();

  std::cerr << "Done.\n";
}
