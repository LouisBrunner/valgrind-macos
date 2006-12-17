
#define FULLTXT 1

#define exec_op glue(exec_, OP)
#define exec_opl glue(glue(exec_, OP), l)
#define exec_opw glue(glue(exec_, OP), w)
#define exec_opb glue(glue(exec_, OP), b)

#define EXECOP2(size, res, s1, flags) \
    asm ("push %4\n\t"\
         "popf\n\t"\
         stringify(OP) size " %" size "2, %" size "0\n\t" \
         "pushf\n\t"\
         "popl %1\n\t"\
         : "=q" (res), "=g" (flags)\
         : "q" (s1), "0" (res), "1" (flags));

#define EXECOP1(size, res, flags) \
    asm ("push %3\n\t"\
         "popf\n\t"\
         stringify(OP) size " %" size "0\n\t" \
         "pushf\n\t"\
         "popl %1\n\t"\
         : "=q" (res), "=g" (flags)\
         : "0" (res), "1" (flags));

#ifdef OP1
inline void exec_opl(int s0, int s1, int iflags)
{
    int res, flags;
    res = s0;
    flags = iflags;
    EXECOP1("", res, flags);
    if (FULLTXT)
       printf("%-6s A=%08x R=%08x CCIN=%04x CC=%04x\n",
              stringify(OP) "l", s0, res, iflags, flags & CC_MASK);
    else
       printf("%08x %04x %04x\n",
               res, iflags, flags & CC_MASK);

}
inline void exec_opw(int s0, int s1, int iflags)
{
    int res, flags;
    res = s0;
    flags = iflags;
    EXECOP1("w", res, flags);
    if (FULLTXT)
       printf("%-6s A=%08x R=%08x CCIN=%04x CC=%04x\n",
              stringify(OP) "w", s0, res, iflags, flags & CC_MASK);
    else
       printf("%08x %04x %04x\n",
              res, iflags, flags & CC_MASK);

}
inline void exec_opb(int s0, int s1, int iflags)
{
    int res, flags;
    res = s0;
    flags = iflags;
    EXECOP1("b", res, flags);
    if (FULLTXT)
       printf("%-6s A=%08x R=%08x CCIN=%04x CC=%04x\n",
              stringify(OP) "b", s0, res, iflags, flags & CC_MASK);
    else
       printf("%08x %04x %04x\n",
              res, iflags, flags & CC_MASK);

}
#else
inline void exec_opl(int s0, int s1, int iflags)
{
    int res, flags;
    res = s0;
    flags = iflags;
    EXECOP2("", res, s1, flags);
    if (FULLTXT)
       printf("%-6s A=%08x B=%08x R=%08x CCIN=%04x CC=%04x\n",
              stringify(OP) "l", s0, s1, res, iflags, flags & CC_MASK);
    else
       printf("%08x %04x %04x\n",
              res, iflags, flags & CC_MASK);
}

inline void exec_opw(int s0, int s1, int iflags)
{
    int res, flags;
    res = s0;
    flags = iflags;
    EXECOP2("w", res, s1, flags);
    if (FULLTXT)
       printf("%-6s A=%08x B=%08x R=%08x CCIN=%04x CC=%04x\n",
              stringify(OP) "w", s0, s1, res, iflags, flags & CC_MASK);
    else
       printf("%08x %04x %04x\n",
              res, iflags, flags & CC_MASK);
}

inline void exec_opb(int s0, int s1, int iflags)
{
    int res, flags;
    res = s0;
    flags = iflags;
    EXECOP2("b", res, s1, flags);
    if (FULLTXT)
       printf("%-6s A=%08x B=%08x R=%08x CCIN=%04x CC=%04x\n",
              stringify(OP) "b", s0, s1, res, iflags, flags & CC_MASK);
    else
       printf("%08x %04x %04x\n",
              res, iflags, flags & CC_MASK);
}
#endif

void exec_op(int s0, int s1)
{
#if 1
  int o,s,z,a,c,p,flags_in;
  for (o = 0; o < 2; o++) {
  for (s = 0; s < 2; s++) {
  for (z = 0; z < 2; z++) {
  for (a = 0; a < 2; a++) {
  for (c = 0; c < 2; c++) {
  for (p = 0; p < 2; p++) {

    flags_in = (o ? CC_O : 0)
             | (s ? CC_S : 0)
             | (z ? CC_Z : 0)
             | (a ? CC_A : 0)
             | (c ? CC_C : 0)
             | (p ? CC_P : 0);
    exec_opl(s0, s1, flags_in);
    exec_opw(s0, s1, flags_in);
    exec_opb(s0, s1, flags_in);
  }}}}}}
#else
    exec_opl(s0, s1, 0);
    exec_opw(s0, s1, 0);
    exec_opb(s0, s1, 0);
    exec_opl(s0, s1, CC_C);
    exec_opw(s0, s1, CC_C);
    exec_opb(s0, s1, CC_C);
#endif
}

void glue(test_, OP)(void)
{
#define NVALS 57
   int i, j;
   static unsigned int val[NVALS]
    = { 0x00, 0x01, 0x02, 0x03, 
        0x3F, 0x40, 0x41, 
        0x7E, 0x7F, 0x80, 0x81, 0x82, 
        0xBF, 0xC0, 0xC1, 
        0xFC, 0xFD, 0xFE, 0xFF, 

        0xFF00, 0xFF01, 0xFF02, 0xFF03, 
        0xFF3F, 0xFF40, 0xFF41, 
        0xFF7E, 0xFF7F, 0xFF80, 0xFF81, 0xFF82, 
        0xFFBF, 0xFFC0, 0xFFC1, 
        0xFFFC, 0xFFFD, 0xFFFE, 0xFFFF, 

        0xFFFFFF00, 0xFFFFFF01, 0xFFFFFF02, 0xFFFFFF03, 
        0xFFFFFF3F, 0xFFFFFF40, 0xFFFFFF41, 
        0xFFFFFF7E, 0xFFFFFF7F, 0xFFFFFF80, 0xFFFFFF81, 0xFFFFFF82, 
        0xFFFFFFBF, 0xFFFFFFC0, 0xFFFFFFC1, 
        0xFFFFFFFC, 0xFFFFFFFD, 0xFFFFFFFE, 0xFFFFFFFF
      };

    exec_op(0x12345678, 0x812FADA);
    exec_op(0x12341, 0x12341);
    exec_op(0x12341, -0x12341);
    exec_op(0xffffffff, 0);
    exec_op(0xffffffff, -1);
    exec_op(0xffffffff, 1);
    exec_op(0xffffffff, 2);
    exec_op(0x7fffffff, 0);
    exec_op(0x7fffffff, 1);
    exec_op(0x7fffffff, -1);
    exec_op(0x80000000, -1);
    exec_op(0x80000000, 1);
    exec_op(0x80000000, -2);
    exec_op(0x12347fff, 0);
    exec_op(0x12347fff, 1);
    exec_op(0x12347fff, -1);
    exec_op(0x12348000, -1);
    exec_op(0x12348000, 1);
    exec_op(0x12348000, -2);
    exec_op(0x12347f7f, 0);
    exec_op(0x12347f7f, 1);
    exec_op(0x12347f7f, -1);
    exec_op(0x12348080, -1);
    exec_op(0x12348080, 1);
    exec_op(0x12348080, -2);

#if TEST_INTEGER_VERBOSE
    if (1)
    for (i = 0; i < NVALS; i++)
      for (j = 0; j < NVALS; j++)
	exec_op(val[i], val[j]);
#endif

#undef NVALS
}

void *glue(_test_, OP) __init_call = glue(test_, OP);

#undef OP
#undef OP_CC

#undef FULLTXT
