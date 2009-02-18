// This is a test for complicated stack traces.
//
// - In deep-A.vgtest, the stack trace is larger than the asked-for depth
//   (12 vs. 8) so not all of the trace is shown.
// - In deep-B.vgtest, we have --alloc-fn=a6..a12, which means that get_XCon
//   needs to redo the IP getting, because 7 functions get removed from the
//   trace, which is more than the initial overestimate of 3.
// - In deep-C.vgtest, we have --alloc-fn=a3..a12, which means that get_XCon
//   ends up with an empty stack trace after removing all the alloc-fns.
//   It then redoes it. 
// - In deep-D.vgtest, we have --alloc-fn=main..a12, which means we have a
//   stack trace with a single "(below main)" entry.

#include <stdlib.h>

void a12(int n) { malloc(n); }
void a11(int n) { a12(n); }
void a10(int n) { a11(n); }
void a9 (int n) { a10(n); }
void a8 (int n) { a9 (n); }
void a7 (int n) { a8 (n); }
void a6 (int n) { a7 (n); }
void a5 (int n) { a6 (n); }
void a4 (int n) { a5 (n); }
void a3 (int n) { a4 (n); }
void a2 (int n) { a3 (n); }
void a1 (int n) { a2 (n); }

int main(void)
{
   int i;

   // This one exceeds the default --depth.
   for (i = 0; i < 10; i++)
      a1(400);    // divisible by 16 -- no slop

   return 0;
}
