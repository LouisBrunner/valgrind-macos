typedef enum {
   TO_NEAREST=0,
   TO_ZERO,
   TO_PLUS_INFINITY,
   TO_MINUS_INFINITY
} round_mode_t;

char *round_mode_name[] = { "near", "zero", "+inf", "-inf" };

void set_rounding_mode(round_mode_t mode)
{
   switch(mode) {
      case TO_NEAREST:
         __asm__ __volatile__(
            "cfc1 $t0, $31"  "\n\t"
            "srl  $t0, 2"    "\n\t"
            "sll  $t0, 2"    "\n\t"
            "ctc1 $t0, $31"  "\n\t"
            :
            :
            : "t0"
         );
         break;
      case TO_ZERO:
         __asm__ __volatile__(
            "cfc1  $t0, $31"  "\n\t"
            "srl   $t0, 2"    "\n\t"
            "sll   $t0, 2"    "\n\t"
            "addiu $t0, 1"    "\n\t"
            "ctc1  $t0, $31"  "\n\t"
            :
            :
            : "t0"
         );
         break;
      case TO_PLUS_INFINITY:
         __asm__ __volatile__(
            "cfc1  $t0, $31"  "\n\t"
            "srl   $t0, 2"    "\n\t"
            "sll   $t0, 2"    "\n\t"
            "addiu $t0, 2"    "\n\t"
            "ctc1  $t0, $31"  "\n\t"
            :
            :
            : "t0"
         );
         break;
      case TO_MINUS_INFINITY:
         __asm__ __volatile__(
            "cfc1  $t0, $31"  "\n\t"
            "srl   $t0, 2"    "\n\t"
            "sll   $t0, 2"    "\n\t"
            "addiu $t0, 3"    "\n\t"
            "ctc1  $t0, $31"  "\n\t"
            :
            :
            : "t0"
         );
         break;
      }
}
