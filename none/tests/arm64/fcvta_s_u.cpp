/*
 * An extra testcase for bug489338
 * The testcase supplied only tests op-codes used by round()
 */

#include <type_traits>
#include <cassert>
#include <iostream>

template<typename T, typename U>
void test_fcvtas(T input, U expected)
{
   U result;
   U* rp(&result);
   T* ip(&input);
   if constexpr (std::is_same_v<double, T> == true)
   {
      if constexpr (std::is_same_v<long, U> == true)
      {
         // double to long
         __asm__ __volatile__(
            "ldr d0, [%1];\n"
            "fcvtas x0, d0;\n"
            "str x0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "d0", "x0");
          assert(result == expected);
      }
      else
      {
         // double to int
         __asm__ __volatile__(
            "ldr d0, [%1];\n"
            "fcvtas w0, d0;\n"
            "str w0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "d0", "x0");
          assert(result == expected);
      }
   }
   else
   {
      if constexpr (std::is_same_v<long, U> == true)
      {
         // float to long
         __asm__ __volatile__(
            "ldr s0, [%1];\n"
            "fcvtas x0, s0;\n"
            "str x0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "s0", "x0");
          assert(result == expected);
      }
      else
      {
         // float to int
         __asm__ __volatile__(
            "ldr s0, [%1];\n"
            "fcvtas w0, s0;\n"
            "str w0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "s0", "w0");
          assert(result == expected);
      }
   }
}

template<typename T, typename U>
void test_fcvtau(T input, U expected)
{
   U result;
   U* rp(&result);
   T* ip(&input);
   if constexpr (std::is_same_v<double, T> == true)
   {
      if constexpr (std::is_same_v<unsigned long, U> == true)
      {
         // double to unsigned long
         __asm__ __volatile__(
            "ldr d0, [%1];\n"
            "fcvtau x0, d0;\n"
            "str x0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "d0", "x0");
          assert(result == expected);
      }
      else
      {
         // double to unsigned int
         __asm__ __volatile__(
            "ldr d0, [%1];\n"
            "fcvtau w0, d0;\n"
            "str w0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "d0", "w0");
          assert(result == expected);
      }
   }
   else
   {
      if constexpr (std::is_same_v<unsigned long, U> == true)
      {
         // float to unsigned long
         __asm__ __volatile__(
            "ldr s0, [%1];\n"
            "fcvtau x0, s0;\n"
            "str x0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "s0", "x0");
          assert(result == expected);
      }
      else
      {
         // float to unsigned int
         __asm__ __volatile__(
            "ldr s0, [%1];\n"
            "fcvtau w0, s0;\n"
            "str w0, [%0];\n"
            : "+rm" (rp)
            : "r" (ip)
            : "memory", "s0", "w0");
          assert(result == expected);
      }
   }
}


int main()
{
    // round "away from zero"
    test_fcvtas(1.5, 2L);
    test_fcvtas(2.5, 3L);
    test_fcvtas(-1.5, -2L);
    test_fcvtas(-2.5, -3L);
    test_fcvtas(0.0, 0L);

    test_fcvtas(1.5, 2);
    test_fcvtas(2.5, 3);
    test_fcvtas(-1.5, -2);
    test_fcvtas(-2.5, -3);
    test_fcvtas(0.0, 0);

    test_fcvtas(1.5F, 2L);
    test_fcvtas(2.5F, 3L);
    test_fcvtas(-1.5F, -2L);
    test_fcvtas(-2.5F, -3L);
    test_fcvtas(0.0F, 0L);

    test_fcvtas(1.5F, 2);
    test_fcvtas(2.5F, 3);
    test_fcvtas(-1.5F, -2);
    test_fcvtas(-2.5F, -3);
    test_fcvtas(0.0F, 0);

    test_fcvtau(1.5, 2UL);
    test_fcvtau(2.5, 3UL);
    test_fcvtau(0.0, 0UL);

    test_fcvtau(1.5, 2U);
    test_fcvtau(2.5, 3U);
    test_fcvtau(0.0, 0U);

    test_fcvtau(1.5F, 2UL);
    test_fcvtau(2.5F, 3UL);
    test_fcvtau(0.0F, 0UL);

    test_fcvtau(1.5F, 2U);
    test_fcvtau(2.5F, 3U);
    test_fcvtau(0.0F, 0U);
}

