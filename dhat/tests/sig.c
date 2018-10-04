// This test implements sorting of a tree involving a mix of significant and
// insignificant nodes. The layout of these functions matches the layout of
// the tree produced by dh_view.js, when sorted by "total bytes".

#include <stdlib.h>

#define F(f, parent)    void* f(size_t n) { return parent(n); }

F(am, malloc)
   // main
   F(a2, am) // main
   F(a3, am)
      // main
      // main
F(bm, malloc)
   // main
   F(b2, bm) // main
   F(b3, bm)
      // main
      // main
F(cm, malloc)
   // main
   F(c2, cm) // main
   F(c3, cm)
      // main
      // main
F(dm, malloc)
   // main
   F(d2, dm) // main
   F(d3, dm)
      // main
      // main

char access(char* p, size_t n)
{
   for (int i = 0; i < 1499; i++) {
      for (int j = 0; j < n; j++) {
         p[j] = j;
      }
   }
   char x = 0;
   for (int j = 0; j < n; j++) {
      x += p[j];
   }
   return x;
}

int main(void)
{

   char* p;

   // Call all the leaves in the above tree. The pointers we pass to access()
   // become significant in a high-access sort and insignificant in a
   // zero-reads-or-zero-writes sort, and vice versa.

   p = am(11); access(p, 11);
   p = a2(10); access(p, 10);
   p = a3(5);  access(p, 5);
   p = a3(4);  access(p, 5);

   p = bm(10); access(p, 10);
   p = b2(9);  access(p, 9);
   p = b3(5);
   p = b3(3);

   p = cm(9); access(p, 9);
   p = c2(8);
   p = c3(4);
   p = c3(3);

   p = dm(8);
   p = d2(7);
   p = d3(4);
   p = d3(2);
}
