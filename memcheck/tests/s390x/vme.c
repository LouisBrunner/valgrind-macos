#define VECTOR __attribute__((vector_size(16)))

typedef unsigned char VECTOR uchar_v;

volatile char     tmp;
static const char use_idx[] = "01234567890abcdefghijklmnopqrstu";

static void depend_on(uchar_v v)
{
   int val = 0;
   for (int i = 0; i < 16; i++)
      val += v[i];
   tmp = use_idx[val & 31];
}

static void pretend_write(uchar_v* v) { __asm__("" : "=m"(*v) : :); }

enum evenodd { even, odd };

static uchar_v
init_vec(uchar_v v, unsigned char es, enum evenodd e, unsigned char val)
{
   int mask = 1 << es;
   int last = (mask - 1) & 15;

   for (int i = 0; i < 16; i++) {
      if ((i & mask) == (e == even ? 0 : mask))
         v[i] = ((i & last) == last) ? val : 0;
   }
   return v;
}

#define GEN_TEST2(mnem, es)                                                    \
   static void test_##mnem##_##es(uchar_v x, enum evenodd e)                   \
   {                                                                           \
      uchar_v res;                                                             \
      uchar_v a = init_vec(x, es, e, 2);                                       \
      uchar_v b = init_vec(x, es, e, 3);                                       \
      __asm__(#mnem " %[v1],%[v2],%[v3]," #es                                  \
              : [v1] "=v"(res)                                                 \
              : [v2] "v"(a), [v3] "v"(b));                                     \
      depend_on(res);                                                          \
   }

#define GEN_TEST3(mnem, es)                                                    \
   static void test_##mnem##_##es(uchar_v x, enum evenodd e)                   \
   {                                                                           \
      uchar_v z = { 0 };                                                  \
      uchar_v res;                                                             \
      uchar_v a = init_vec(x, es, e, 2);                                       \
      uchar_v b = init_vec(x, es, e, 3);                                       \
      uchar_v c = init_vec(z, es, e, 4);                                       \
      __asm__(#mnem " %[v1],%[v2],%[v3],%[v4]," #es                            \
              : [v1] "=v"(res)                                                 \
              : [v2] "v"(a), [v3] "v"(b), [v4] "v"(c));                        \
      depend_on(res);                                                          \
   }

GEN_TEST2(vme, 0)
GEN_TEST2(vme, 1)
GEN_TEST2(vme, 2)
GEN_TEST2(vmo, 2)
GEN_TEST2(vmle, 1)
GEN_TEST2(vmlo, 2)

GEN_TEST3(vmae, 0)
GEN_TEST3(vmale, 1)
GEN_TEST3(vmao, 2)
GEN_TEST3(vmalo, 2)

static void do_valid(uchar_v x)
{
   test_vme_0(x, even);
   test_vme_1(x, even);
   test_vme_2(x, even);
   test_vmo_2(x, odd);
   test_vmle_1(x, even);
   test_vmlo_2(x, odd);
   test_vmae_0(x, even);
   test_vmale_1(x, even);
   test_vmao_2(x, odd);
   test_vmalo_2(x, odd);
}

static void do_invalid(uchar_v x)
{
   test_vme_0(x, odd);
   test_vme_1(x, odd);
   test_vme_2(x, odd);
   test_vmo_2(x, even);
   test_vmale_1(x, odd);
}

int main(void)
{
   uchar_v x;
   pretend_write(&x);
   do_valid(x);
   do_invalid(x);
   return 0;
}
