
   // Comment "both" means tp[0] and tp[-1] are both bad.
   // Otherwise only tp[-1] is bad.

   #define TTT \
      if (__builtin_setjmp(TTT_jmpbuf) == 0) \
      { fprintf(stderr,  "about to do %d [0]\n", __LINE__); tn = tp[ 0]; } \
      if (__builtin_setjmp(TTT_jmpbuf) == 0) \
      { fprintf(stderr, "about to do %d [-1]\n", __LINE__); tn = tp[-1]; }

   #define b(    a,  c)   tp = (long*)a;                    TTT
   #define ui(op, a,  c)  tp = (long*)op(long)a;            TTT
   #define g(op, a,b,c)   tp = (long*)((long)a op (long)b); TTT
   #define UNU            __attribute__((unused))

   struct sigaction sigsegv;
   // Scratch values
   long  a, tn;
   long* tp;
   
   // Known pointers
   long* p = malloc(sizeof(long)*10);  UNU long* p2 = malloc(sizeof(long)*10);
   UNU long* pp = p;
   // Unknown pointers
//   long up[10], UNU up2[10];

   // Known nonptrs;  make them zero and known
   long n = a ^ a, UNU n2 = n+1, UNU n7F = 0x7fffffffUL, UNU nFF = ~n;
   
   // Unknown nonptrs;  make them zero but unknown
   long un = 0x01100000UL, UNU un2 = un;

   // Known nonptr, from pointerness range check
   UNU long nn = 0;

   // Intall SEGV handler 
   memset(&sigsegv, 0, sizeof(sigsegv));
   sigsegv.sa_handler = SEGV_handler;
   sigsegv.sa_flags   = SA_NODEFER; /* so we can handle signal many times */
   assert( 0 == sigemptyset( &sigsegv.sa_mask ) );
   assert( 0 == sigaction(SIGSEGV, &sigsegv, NULL) );
