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
   a1(100);
   a1(100);
   a1(100);
   b1(100);
   c1(100);
   d1(100);
   d2(100);
   d3(100);
   d4(100);
   return 0;
}
