// This module does unit testing of m_libcbase.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "coregrind/m_libcbase.c"


void test_isXYZ(void)
{
   assert(   VG_(isspace)(' ') );
   assert(   VG_(isspace)('\n') );
   assert(   VG_(isspace)('\t') );
   assert( ! VG_(isspace)('3') );
   assert( ! VG_(isspace)('x') );

   assert(   VG_(isdigit)('0') );
   assert(   VG_(isdigit)('1') );
   assert(   VG_(isdigit)('5') );
   assert(   VG_(isdigit)('9') );
   assert( ! VG_(isdigit)('a') );
   assert( ! VG_(isdigit)('!') );
}
 
void test_is_XYZ_digit()
{
   Long x;

   assert( is_dec_digit('0', &x) && 0 == x );
   assert( is_dec_digit('1', &x) && 1 == x );
   assert( is_dec_digit('9', &x) && 9 == x );

   assert( is_hex_digit('0', &x) &&  0 == x );
   assert( is_hex_digit('1', &x) &&  1 == x );
   assert( is_hex_digit('9', &x) &&  9 == x );
   assert( is_hex_digit('a', &x) && 10 == x );
   assert( is_hex_digit('f', &x) && 15 == x );
   assert( is_hex_digit('A', &x) && 10 == x );
   assert( is_hex_digit('F', &x) && 15 == x );
}

void test_strtoll(void)
{
   // For VG_(strtoll*)()
   typedef struct {
      Char* str;        // The string to convert.
      Long  res;        // The result.
      Char  endptr_val; // The char one past the end of the converted text.
   } StrtollInputs;

   // VG_(strtoll10)()
   {
      StrtollInputs a[] = {
         // If there's no number at the head of the string, return 0, and
         // make 'endptr' point to the start of the string.
         { str : "",      res : 0, endptr_val : '\0' },
         { str : " \n\t", res : 0, endptr_val : ' '  },
         { str : "one",   res : 0, endptr_val : 'o'  },
         { str : "\ntwo", res : 0, endptr_val : '\n' },

         // Successful conversion.  Leading whitespace is ignored.  A single
         // '-' or '+' is accepted.
         { str :  "0",            res :       0, endptr_val : '\0' },
         { str : "+0",            res :       0, endptr_val : '\0' },
         { str : "-0",            res :       0, endptr_val : '\0' },
         { str :  "1",            res :       1, endptr_val : '\0' },
         { str : "+1",            res :       1, endptr_val : '\0' },
         { str : "-1",            res :      -1, endptr_val : '\0' },
         { str : "12",            res :      12, endptr_val : '\0' },
         { str : "-567",          res :    -567, endptr_val : '\0' },
         { str : "1234567",       res : 1234567, endptr_val : '\0' },
         { str : "007",           res :       7, endptr_val : '\0' },
         { str : "   +42",        res :      42, endptr_val : '\0' },
         { str : "\n\t\r\v  -56", res :     -56, endptr_val : '\0' },
         { str : "123xyz",        res :     123, endptr_val : 'x'  },
         { str : " -123abc",      res :    -123, endptr_val : 'a'  },

         // Whitespace after the +/- is not allowed;  conversion fails.
         { str : "+ 1",   res :  0, endptr_val : '+' },
         { str : "-\n1",  res :  0, endptr_val : '-' },
      };

      // Nb: We test the results against strtoll() as well.
      int i;
      for (i = 0; i < (sizeof(a) / sizeof(StrtollInputs)); i++) {
         Char* endptr1;
         char* endptr2;
         Long      res1 = VG_(strtoll10)(a[i].str, &endptr1);
         long long res2 =     strtoll   (a[i].str, &endptr2, 10);
         //printf("res1 = %lld, *endptr1 = '%c'\n", res1, *endptr1);
         //printf("res2 = %lld, *endptr2 = '%c'\n", res2, *endptr2);
         assert(a[i].res == res1 && a[i].endptr_val == *endptr1);
         assert(res2     == res1 && *endptr2        == *endptr1);
      }
   }

   // VG_(strtoll16)()
   {
      StrtollInputs a[] = {
         // If there's no number at the head of the string, return 0, and
         // make 'endptr' point to the start of the string.
         { str : "",      res : 0, endptr_val : '\0' },
         { str : " \n\t", res : 0, endptr_val : ' '  },
         { str : "one",   res : 0, endptr_val : 'o'  },
         { str : "\ntwo", res : 0, endptr_val : '\n' },

         // Successful conversion.  Leading whitespace is ignored.  A single
         // '-' or '+' is accepted.  "0X" and "0x" are also allowed at the
         // front, but if no digits follow, just the "0" is converted.
         { str :   "0",           res :        0, endptr_val : '\0' },
         { str : "0x0",           res :        0, endptr_val : '\0' },
         { str : "0X0",           res :        0, endptr_val : '\0' },
         { str : "0x",            res :        0, endptr_val : 'x'  },
         { str : "0Xg",           res :        0, endptr_val : 'X'  },
         { str :   "0",           res :        0, endptr_val : '\0' },
         { str :  "+0",           res :        0, endptr_val : '\0' },
         { str :  "-0",           res :        0, endptr_val : '\0' },
         { str :   "1",           res :        1, endptr_val : '\0' },
         { str :  "+1",           res :        1, endptr_val : '\0' },
         { str :  "-1",           res :       -1, endptr_val : '\0' },
         { str :  "1a",           res :       26, endptr_val : '\0' },
         { str : "-5F7",          res :    -1527, endptr_val : '\0' },
         { str : "0x1234567",     res : 19088743, endptr_val : '\0' },
         { str : "007",           res :        7, endptr_val : '\0' },
         { str : "0X00ABCD",      res :    43981, endptr_val : '\0' },
         { str : "   +AbC",       res :     2748, endptr_val : '\0' },
         { str : "   -0xAbC",     res :    -2748, endptr_val : '\0' },
         { str : "   -0xxx",      res :        0, endptr_val : 'x'  },
         { str : "\n\t\r\v  -56", res :      -86, endptr_val : '\0' },
         { str : "123xyz",        res :      291, endptr_val : 'x'  },
         { str : " -123defghi",   res : -1195503, endptr_val : 'g'  },

         // Whitespace after the +/- is not allowed;  conversion fails.
         { str : "+ 1",    res :  0, endptr_val : '+' },
         { str : "-\n0x1", res :  0, endptr_val : '-' },
      };

      // Nb: We test the results against strtoll() as well.
      int i;
      for (i = 0; i < (sizeof(a) / sizeof(StrtollInputs)); i++) {
         Char* endptr1;
         char* endptr2;
         Long      res1 = VG_(strtoll16)(a[i].str, &endptr1);
         long long res2 =     strtoll   (a[i].str, &endptr2, 16);
         //printf("  res1 = %lld, *endptr1 = '%c'\n", res1, *endptr1);
         //printf("  res2 = %lld, *endptr2 = '%c'\n", res2, *endptr2);
         assert(a[i].res == res1 && a[i].endptr_val == *endptr1);
         assert(res2     == res1 && *endptr2        == *endptr1);
      }
   }
   // VG_(strtod)()
   {
      StrtollInputs a[] = {
         // If there's no number at the head of the string, return 0, and
         // make 'endptr' point to the start of the string.
         { str : "",      res : 0, endptr_val : '\0' },
         { str : " \n\t", res : 0, endptr_val : ' '  },
         { str : "one",   res : 0, endptr_val : 'o'  },
         { str : "\ntwo", res : 0, endptr_val : '\n' },

         // Successful conversion.  Leading whitespace is ignored.  A single
         // '-' or '+' is accepted.  "0X" and "0x" are also allowed at the
         // front, but if no digits follow, just the "0" is converted.
         { str :   "0",           res :        0, endptr_val : '\0' },
         { str :   "0",           res :        0, endptr_val : '\0' },
         { str :  "+0",           res :        0, endptr_val : '\0' },
         { str :  "-0",           res :        0, endptr_val : '\0' },
         { str :   "1",           res :        1, endptr_val : '\0' },
         { str :  "+1",           res :        1, endptr_val : '\0' },
         { str :  "-1",           res :       -1, endptr_val : '\0' },
         { str :  "1a",           res :       26, endptr_val : '\0' },
         { str : "-5F7",          res :    -1527, endptr_val : '\0' },
         { str : "0x1234567",     res : 19088743, endptr_val : '\0' },
         { str : "007",           res :        7, endptr_val : '\0' },
         { str : "0X00ABCD",      res :    43981, endptr_val : '\0' },
         { str : "   +AbC",       res :     2748, endptr_val : '\0' },
         { str : "   -0xAbC",     res :    -2748, endptr_val : '\0' },
         { str : "   -0xxx",      res :        0, endptr_val : 'x'  },
         { str : "\n\t\r\v  -56", res :      -86, endptr_val : '\0' },
         { str : "123xyz",        res :      291, endptr_val : 'x'  },
         { str : " -123defghi",   res : -1195503, endptr_val : 'g'  },

         // Whitespace after the +/- is not allowed;  conversion fails.
         { str : "+ 1",    res :  0, endptr_val : '+' },
         { str : "-\n0x1", res :  0, endptr_val : '-' },
      };

      // Nb: We test the results against strtoll() as well.
      int i;
      for (i = 0; i < (sizeof(a) / sizeof(StrtollInputs)); i++) {
         Char* endptr1;
         char* endptr2;
         Long      res1 = VG_(strtoll16)(a[i].str, &endptr1);
         long long res2 =     strtoll   (a[i].str, &endptr2, 16);
         //printf("  res1 = %lld, *endptr1 = '%c'\n", res1, *endptr1);
         //printf("  res2 = %lld, *endptr2 = '%c'\n", res2, *endptr2);
         assert(a[i].res == res1 && a[i].endptr_val == *endptr1);
         assert(res2     == res1 && *endptr2        == *endptr1);
      }
   }
}

int main(void)
{
   //--------------------------------------------------------------------
   // Macros in pub_tool_libcbase.h
   //--------------------------------------------------------------------
   // XXX: todo

   //--------------------------------------------------------------------
   // Char functions
   //--------------------------------------------------------------------
   test_isXYZ();

   //--------------------------------------------------------------------
   // String-to-number functions
   //--------------------------------------------------------------------
   test_is_XYZ_digit();
   test_strtoll();

   //--------------------------------------------------------------------
   // String functions
   //--------------------------------------------------------------------
   // XXX: todo

   //--------------------------------------------------------------------
   // Mem functions
   //--------------------------------------------------------------------
   // XXX: todo

   //--------------------------------------------------------------------
   // Miscellaneous functions
   //--------------------------------------------------------------------
   // XXX: todo
 
   return 0;
}

