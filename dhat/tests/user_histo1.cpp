#include <vector>
#include <cstdint>
#include <iostream>
#include <random>
#include "dhat/dhat.h"

int main()
{
   std::vector<uint8_t> vec(2000, 0);
   DHAT_HISTOGRAM_MEMORY(vec.data());
   std::mt19937 gen(42);;
   std::uniform_int_distribution<> index_distrib(0, 1999);
   std::uniform_int_distribution<> val_distrib(0, 255);
   
   for (int i = 0; i < 20; ++i)
   {
      int index = index_distrib(gen);
      int val = val_distrib(gen);
      vec[index] = val;
      //std::cout << "wrote " << val << " to index " << index << "\n";
   }
   
   // try to generate some warnings
   vec.resize(500);
   vec.shrink_to_fit();
   DHAT_HISTOGRAM_MEMORY(vec.data());
   
   auto old = vec.data();
   vec.resize(100000);
   // old should have been deleted
   DHAT_HISTOGRAM_MEMORY(old);
   // and this is too big
   DHAT_HISTOGRAM_MEMORY(vec.data());
}
