// This test implements a moderately complex call tree. The layout of these
// functions matches the layout of the tree produced by dh_view.js, when
// sorted by "total bytes".

#include <stdlib.h>

#define F(f, parent)    void* f(size_t n) { return parent(n); }

// Note: don't use j1 -- that is a builtin C function, believe it or not.
F(a, malloc)
   F(b1, a)
      F(c1, b1)
         F(d1, c1)
         F(d2, c1)   // insig total-bytes
      F(c2, b1)
   F(b2, a)
   F(b3, a) F(e, b3) F(f, e)
F(g, malloc) F(h, g) F(i, h)
   F(j2, i) F(k, j2) F(l, k)
   F(j3, i) F(m, j3)
      F(n1, m)
      F(n2, m) F(o, n2)
   F(p, i) F(q, p)   // insig total-bytes
   F(r, i)           // insig total-bytes
F(s1, malloc) F(s2, s1) F(s3, s2) F(s4, s3) F(s5, s4)
F(t, malloc)
F(u, malloc)
F(v, malloc)
   F(w, v)           // insig total-bytes
   F(x, v)           // insig total-bytes
   F(y, v)           // insig total-bytes
   F(z, v)           // insig total-bytes

int main(void)
{
   // Call all the leaves in the above tree.

   int* d1p = d1(706);
   free(d1p); // So the t-final numbers differ from the t-gmax/total numbers.

   d2(5);
   c2(30);
   b2(20);
   f(10);
   l(60);
   n1(30);
   o(20);
   q(7);
   r(3);
   s5(30);
   t(20);
   u(19);
   w(9);
   x(8);
   y(7);
   z(5);
   z(1);

   // And one allocation directly from main().
   malloc(10);
}
