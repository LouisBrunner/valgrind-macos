
int main ( void )
{
#if defined(__powerpc64__) || defined(_AIX)
   /* on ppc64-linux, a function pointer points to a function
      descriptor, not to the function's entry point.  Hence to get
      uniform behaviour on all supported targets - a jump to 0xE000000
      - the following is needed. */
   unsigned long long int fake_fndescr[3];
   fake_fndescr[0] = 0xE000000;
   fake_fndescr[1] = 0;
   fake_fndescr[2] = 0;
   return ((int(*)(void)) fake_fndescr) ();
#else
   char* p = (char*)0xE000000;
   return ((int(*)(void)) p) ();
#endif
}
