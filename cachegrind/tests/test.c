int main(void) {
   int z = 0;
   for (int i = 0; i < 1000000; i++) {
      z += i;
   }
   return z % 256;
}
