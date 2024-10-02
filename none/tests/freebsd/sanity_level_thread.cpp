#include <string>
#include <fstream>
#include <thread>

using namespace std;

ofstream output("output.txt");

void task(string msg)
{
    output << "task msg: " << msg;
}

int main()
{
    thread t1(task, "Hello\n");
    thread t2(task, "World\n");

    t1.join();
    t2.join();
}
