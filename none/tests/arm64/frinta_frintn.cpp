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
void test_frinta_fullvec(T* input, T* expected)
{
    T result[2*sizeof(double)/sizeof(T)];
    T* rp = result;
    if constexpr (std::is_same_v<double, T> == true)
    {
     __asm__ __volatile__(
         "ldr q23, [%1];\n"
         "frinta v22.2d, v23.2d;\n"
         "str q22, [%0];\n"
         : "+rm" (rp)
         : "r" (input)
         : "memory", "v22", "v23");
       assert(result[0] == expected[0]);
       assert(result[1] == expected[1]);
   }
   else
   {
     __asm__ __volatile__(
         "ldr q23, [%1];\n"
         "frinta v22.4s, v23.4s;\n"
         "str q22, [%0];\n"
         : "+rm" (rp)
         : "r" (input)
         : "memory", "v22", "v23");
       assert(result[0] == expected[0]);
       assert(result[1] == expected[1]);
       assert(result[2] == expected[2]);
       assert(result[3] == expected[3]);
   }
}

void test_frinta_halfvec(float* input, float* expected)
{
    float result[2];
    float* rp = result;
    __asm__ __volatile__(
         "ldr d23, [%1];\n"
         "frinta v22.2s, v23.2s;\n"
         "str d22, [%0];\n"
         : "+rm" (rp)
         : "r" (input)
         : "memory", "v22", "v23");
   assert(result[0] == expected[0]);
   assert(result[1] == expected[1]);
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

template<typename T>
void test_frintn_fullvec(T* input, T* expected)
{
    T result[2*sizeof(double)/sizeof(T)];
    T* rp = result;
    if constexpr (std::is_same_v<double, T> == true)
    {
     __asm__ __volatile__(
         "ldr q23, [%1];\n"
         "frintn v22.2d, v23.2d;\n"
         "str q22, [%0];\n"
         : "+rm" (rp)
         : "r" (input)
         : "memory", "v22", "v23");
       assert(result[0] == expected[0]);
       assert(result[1] == expected[1]);
   }
   else
   {
     __asm__ __volatile__(
         "ldr q23, [%1];\n"
         "frintn v22.4s, v23.4s;\n"
         "str q22, [%0];\n"
         : "+rm" (rp)
         : "r" (input)
         : "memory", "v22", "v23");
       assert(result[0] == expected[0]);
       assert(result[1] == expected[1]);
       assert(result[2] == expected[2]);
       assert(result[3] == expected[3]);
   }
}

void test_frintn_halfvec(float* input, float* expected)
{
    float result[2];
    float* rp = result;
     __asm__ __volatile__(
         "ldr d23, [%1];\n"
         "frintn v22.2s, v23.2s;\n"
         "str d22, [%0];\n"
         : "+rm" (rp)
         : "r" (input)
         : "memory", "v22", "v23");
   assert(result[0] == expected[0]);
   assert(result[1] == expected[1]);
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

    double in1[] = {1.5, 1.5};
    double out1[] = {2.0, 2,0};
    test_frinta_fullvec(in1, out1);
    double in2[] = {2.5, 2.5};
    double out2[] = {3.0, 3,0};
    test_frinta_fullvec(in2, out2);
    double in3[] = {-1.5, -1.5};
    double out3[] = {-2.0, -2,0};
    test_frinta_fullvec(in3, out3);
    double in4[] = {-2.5, -2.5};
    double out4[] = {-3.0, -3,0};
    test_frinta_fullvec(in4, out4);

    float in1f[] = {1.5F, 1.5F, 1.5F, 1.5F};
    float out1f[] = {2.0F, 2.0F, 2.0F, 2.0F};
    test_frinta_fullvec(in1f, out1f);
    test_frinta_halfvec(in1f, out1f);
    float in2f[] = {2.5F, 2.5F, 2.5F, 2.5F};
    float out2f[] = {3.0F, 3.0F, 3.0F, 3.0F};
    test_frinta_fullvec(in2f, out2f);
    test_frinta_halfvec(in2f, out2f);
    float in3f[] = {-1.5F, -1.5F, -1.5F, -1.5F};
    float out3f[] = {-2.0F, -2.0F, -2.0F, -2.0F};
    test_frinta_fullvec(in3f, out3f);
    test_frinta_halfvec(in3f, out3f);
    float in4f[] = {-2.5F, -2.5F, -2.5F, -2.5F};
    float out4f[] = {-3.0F, -3.0F, -3.0F, -3.0F};
    test_frinta_fullvec(in4f, out4f);
    test_frinta_halfvec(in4f, out4f);

    // round "to even"
    test_frintn(1.5, 2.0);
    test_frintn(2.5, 2.0);
    test_frintn(-1.5, -2.0);
    test_frintn(-2.5, -2.0);
    test_frintn(1.5F, 2.0F);
    test_frintn(2.5F, 2.0F);
    test_frintn(-1.5F, -2.0F);
    test_frintn(-2.5F, -2.0F);

    test_frintn_fullvec(in1, out1);
    test_frintn_fullvec(in2, out1);
    test_frintn_fullvec(in3, out3);
    test_frintn_fullvec(in4, out3);

    test_frintn_fullvec(in1f, out1f);
    test_frintn_halfvec(in1f, out1f);
    test_frintn_fullvec(in2f, out1f);
    test_frintn_halfvec(in2f, out1f);
    test_frintn_fullvec(in3f, out3f);
    test_frintn_halfvec(in3f, out3f);
    test_frintn_fullvec(in4f, out3f);
    test_frintn_halfvec(in4f, out3f);
}

