#include <stdlib.h>

void a4(int n) { malloc(n); }
void a3(int n) { a4(n); }
void a2(int n) { a3(n); }
void a1(int n) { a2(n); }

void b4(int n) { malloc(n); }
void b3(int n) { b4(n); }
void b2(int n) { b3(n); }
void b1(int n) { b2(n); }

void c4(int n) { malloc(n); }
void c3(int n) { c4(n); }
void c2(int n) { c3(n); }
void c1(int n) { c2(n); }

void d4(int n) { malloc(n); }
void d3(int n) { d4(n); }
void d2(int n) { d3(n); }
void d1(int n) { d2(n); }

int main(void)
{
   a1(400); // We use a number that's a multiple of 16, so there's no slop
   a1(400); // bytes.
   a1(400);
   b1(400);
   c1(400);
   d1(400);
   d2(400);
   d3(400);
   d4(400);
   return 0;
}
