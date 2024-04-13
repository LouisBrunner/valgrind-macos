/*
 * An extra testcase for bug484426
 * The testcase supplied only tests single precision frinta
 */

#include <type_traits>
#include <cassert>

template<typename T>
void test_frinta(T input, T expected)
{
    T result;
    T* rp(&result);
    T* ip(&input);
    if constexpr (std::is_same_v<double, T> == true)
    {
     __asm__ __volatile__(
         "ldr d0, [%1];\n"
         "frinta d0, d0;\n"
         "str d0, [%0];\n"
         : "+rm" (rp)
         : "r" (ip)
         : "memory", "d0");
       assert(result == expected);
   }
   else
   {
    __asm__ __volatile__(
         "ldr s0, [%1];\n"
         "frinta s0, s0;\n"
         "str s0, [%0];\n"
         : "+rm" (rp)
         : "r" (ip)
         : "memory", "s0");
       assert(result == expected);
   }
}

template<typename T>
void test_frintn(T input, T expected)
{
    T result;
    T* rp(&result);
    T* ip(&input);
    if constexpr (std::is_same_v<double, T> == true)
    {
     __asm__ __volatile__(
         "ldr d0, [%1];\n"
         "frintn d0, d0;\n"
         "str d0, [%0];\n"
         : "+rm" (rp)
         : "r" (ip)
         : "memory", "d0");
       assert(result == expected);
   }
   else
   {
    __asm__ __volatile__(
         "ldr s0, [%1];\n"
         "frintn s0, s0;\n"
         "str s0, [%0];\n"
         : "+rm" (rp)
         : "r" (ip)
         : "memory", "s0");
       assert(result == expected);
   }
}

int main()
{
    // round "away from zero"
    test_frinta(1.5, 2.0);
    test_frinta(2.5, 3.0);
    test_frinta(-1.5, -2.0);
    test_frinta(-2.5, -3.0);
    test_frinta(1.5F, 2.0F);
    test_frinta(2.5F, 3.0F);
    test_frinta(-1.5F, -2.0F);
    test_frinta(-2.5F, -3.0F);

    // round "to even"
    test_frintn(1.5, 2.0);
    test_frintn(2.5, 2.0);
    test_frintn(-1.5, -2.0);
    test_frintn(-2.5, -2.0);
    test_frintn(1.5F, 2.0F);
    test_frintn(2.5F, 2.0F);
    test_frintn(-1.5F, -2.0F);
    test_frintn(-2.5F, -2.0F);
}

