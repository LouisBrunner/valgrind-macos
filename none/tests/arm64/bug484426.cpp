#include <cmath>
#include <iostream>

int main(int argc, char** argv)
{
   float value = std::stof(argv[1]);
   std::cout << std::round(value) << "\n";
}
