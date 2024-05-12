#include <assert.h>
#include <string.h>

int main()
{
    char buf[64] __attribute__((aligned(64)));
    unsigned long check_dc_zva;
    unsigned long check_dc_cvap;
    memset(buf, 0xAA, 64);
    asm volatile("mrs %0, dczid_el0" : "=r" (check_dc_zva));
    asm volatile("dc cvac, %0" :: "r" (buf));
    asm volatile("dc cvau, %0" :: "r" (buf));
    asm volatile("mrs %0, id_aa64isar1_el1" : "=r" (check_dc_cvap));
    /* Not sure if GCC supports this syntax */
    /*
    if (check_dc_cvap & 0x3) {
       asm volatile(".arch_extension ccpp;dc cvap, %0" :: "r" (buf));
    }
    if (check_dc_cvap & 0x2) {
      asm volatile(".arch_extension ccdp;dc cvadp, %0" :: "r" (buf));
    }
    */
    asm volatile("dc civac, %0" :: "r" (buf));
    // and while I'm at it
    if (!(check_dc_zva & 0x10)) {
       asm volatile("dc zva, %0" :: "r" (buf));
       assert(buf[0] == 0);
       assert(buf[63] == 0);
   }
}
