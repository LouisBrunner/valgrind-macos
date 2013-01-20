#include <stdio.h>

__attribute__((noinline)) int test_block_size1 ()
{
   int result = 1;
   __asm__ __volatile__(
         ".set noreorder"      "\n\t"
         ".set nomacro"        "\n\t"
         "b begin1"            "\n\t"
         "nop"                 "\n\t"
         "begin1:"             "\n\t"
         ".rept 56"            "\n\t"
         ".word 0"             "\n\t"
         ".endr"               "\n\t"
         "li $4, 0"            "\n\t"
         "j end1"              "\n\t"
         "nop"                 "\n\t"
         "b label1"            "\n\t"
         "nop"                 "\n\t"
         "label1:"             "\n\t"
         "li $4, 1"            "\n\t"
         "end1:"               "\n\t"
         "move %0, $4"         "\n\t"
         ".set reorder"        "\n\t"
         ".set macro"          "\n\t"
         : /*out*/ "=r" (result)
         : /*in*/
         : /*trash*/ "$4");
   return result;
}

__attribute__((noinline)) int test_block_size2 ()
{
   int result = 1;
   __asm__ __volatile__(
         ".set noreorder"      "\n\t"
         ".set nomacro"        "\n\t"
         "b begin2"            "\n\t"
         "nop"                 "\n\t"
         "begin2:"             "\n\t"
         ".rept 58"            "\n\t"
         ".word 0"             "\n\t"
         ".endr"               "\n\t"
         "li $4, 1"            "\n\t"
         "j end2"              "\n\t"
         "li $4, 0"            "\n\t"
         "end2:"               "\n\t"
         "move %0, $4"         "\n\t"
         ".set reorder"        "\n\t"
         ".set macro"          "\n\t"
         : /*out*/ "=r" (result)
         : /*in*/
         : /*trash*/ "$4");
   return result;
}

int main ()
{
   /*******************TEST1*******************/
   if (test_block_size1() == 0)
      printf("test1 - PASS\n");
   else
      printf("test1 - FAIL\n");

   /*******************TEST2*******************/
   if (test_block_size2() == 0)
      printf("test2 - PASS\n");
   else
      printf("test2 - FAIL\n");
   return 0;
}
