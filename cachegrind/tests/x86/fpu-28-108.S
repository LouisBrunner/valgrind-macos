/* Test 28 and 108 byte loads and stores.  (Just make sure program
   runs without any assertion failures from V.) */

/* Useful listing: 
	gcc -o tests/fpu_28_108 tests/fpu_28_108.S -Wa,-a */

#include "tests/asm.h"

.data
fooble:
        .long 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .long 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .long 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
bar:
        
.text
.globl VG_SYM_ASM(main)
VG_SYM_ASM(main):
        fstsw   fooble
        fsave   fooble
        frstor  fooble
        fstenv  fooble
        fldenv  fooble
        movl    $0, %eax
        ret

