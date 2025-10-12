int main(void)
{
   float val = 0.0f;
   __asm__ volatile ("celfbr 1,5,%[f0],0" : : [f0]"f"(val) : "cc", "r1");
   return 0;
}
