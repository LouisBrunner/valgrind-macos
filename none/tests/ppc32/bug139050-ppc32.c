
#include <stdio.h>
#include <assert.h>

typedef  unsigned long long int  ULong;
typedef  unsigned int            UInt;
 
static ULong GetCPU_ClockCyclesSinceStartup(void) 
 { 
   UInt uTimeBaseLow; 
   UInt uTimeBaseHigh; 
   UInt uCheck; 
   __asm__ __volatile__("1:     mfspr %0,269\n\t" 
                        "       mfspr %1,268\n\t" 
                        "       mfspr %2,269\n\t" 
                        "       cmpw   %2, %0\n\t" 
                        "       bne    1b" 
                        : "=r" (uTimeBaseHigh), 
                          "=r" (uTimeBaseLow), 
                          "=r" (uCheck)
                        : /*in*/
                        : /*trash*/ "cr0","cr7" );

   return (((ULong)(uTimeBaseHigh) << 32) | uTimeBaseLow); 
 } 
 
 int main(int argc, char** argv) 
 { 
    ULong cys = GetCPU_ClockCyclesSinceStartup();
    /* implausible that machine has been up less than 4G cycles */
    assert(cys > (1ULL << 32));
    printf("success\n");
    return 0; 
 }
