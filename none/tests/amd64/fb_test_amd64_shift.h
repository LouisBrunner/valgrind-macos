
#define exec_op glue(exec_, OP)
#define exec_opq glue(glue(exec_, OP), q)
#define exec_opl glue(glue(exec_, OP), l)
#define exec_opw glue(glue(exec_, OP), w)
#define exec_opb glue(glue(exec_, OP), b)

#ifndef OP_SHIFTD

#ifdef OP_NOBYTE
#define EXECSHIFT(size, res, s1, s2, flags) \
    asm ("pushq %4\n\t"\
         "popfq\n\t"\
         stringify(OP) size " %" size "2, %" size "0\n\t" \
         "pushfq\n\t"\
         "popq %1\n\t"\
         : "=g" (res), "=g" (flags)\
         : "r" (s1), "0" (res), "1" (flags));
#else
#define EXECSHIFT(size, res, s1, s2, flags) \
    asm ("pushq %4\n\t"\
         "popfq\n\t"\
         stringify(OP) size " %%cl, %" size "0\n\t" \
         "pushfq\n\t"\
         "popq %1\n\t"\
         : "=q" (res), "=g" (flags)\
         : "c" (s1), "0" (res), "1" (flags));
#endif

void exec_opq(int64 s2, int64 s0, int64 s1, int64 iflags)
{
    int64 res, flags;
    res = s0;
    flags = iflags;
    EXECSHIFT("q", res, s1, s2, flags);
    /* overflow is undefined if count != 1 */
    if (s1 != 1)
      flags &= ~CC_O;
    xxprintf("%-10s A=%016llx B=%016llx R=%016llx CCIN=%04llx CC=%04llx\n",
           stringify(OP) "q", s0, s1, res, iflags, flags & CC_MASK);
}

void exec_opl(int64 s2, int64 s0, int64 s1, int64 iflags)
{
    int64 res, flags;
    res = s0;
    flags = iflags;
    EXECSHIFT("", res, s1, s2, flags);
    /* overflow is undefined if count != 1 */
    if (s1 != 1)
      flags &= ~CC_O;
    xxprintf("%-10s A=%016llx B=%016llx R=%016llx CCIN=%04llx CC=%04llx\n",
           stringify(OP) "l", s0, s1, res, iflags, flags & CC_MASK);
}

void exec_opw(int64 s2, int64 s0, int64 s1, int64 iflags)
{
    int64 res, flags;
    res = s0;
    flags = iflags;
    EXECSHIFT("w", res, s1, s2, flags);
    /* overflow is undefined if count != 1 */
    if (s1 != 1)
      flags &= ~CC_O;
    xxprintf("%-10s A=%016llx B=%016llx R=%016llx CCIN=%04llx CC=%04llx\n",
           stringify(OP) "w", s0, s1, res, iflags, flags & CC_MASK);
}

#else
#define EXECSHIFT(size, res, s1, s2, flags) \
    asm ("pushq %4\n\t"\
         "popfq\n\t"\
         stringify(OP) size " %%cl, %" size "5, %" size "0\n\t" \
         "pushfq\n\t"\
         "popq %1\n\t"\
         : "=g" (res), "=g" (flags)\
         : "c" (s1), "0" (res), "1" (flags), "r" (s2));

void exec_opl(int64 s2, int64 s0, int64 s1, int64 iflags)
{
    int64 res, flags;
    res = s0;
    flags = iflags;
    EXECSHIFT("", res, s1, s2, flags);
    /* overflow is undefined if count != 1 */
    if (s1 != 1)
      flags &= ~CC_O;
    xxprintf("%-10s A=%016llx B=%016llx C=%016llx R=%016llx CCIN=%04llx CC=%04llx\n",
           stringify(OP) "l", s0, s2, s1, res, iflags, flags & CC_MASK);
}

void exec_opw(int64 s2, int64 s0, int64 s1, int64 iflags)
{
    int64 res, flags;
    res = s0;
    flags = iflags;
    EXECSHIFT("w", res, s1, s2, flags);
    /* overflow is undefined if count != 1 */
    if (s1 != 1)
      flags &= ~CC_O;
    xxprintf("%-10s A=%016llx B=%016llx C=%016llx R=%016llx CCIN=%04llx CC=%04llx\n",
           stringify(OP) "w", s0, s2, s1, res, iflags, flags & CC_MASK);
}

#endif

#ifndef OP_NOBYTE
void exec_opb(int64 s0, int64 s1, int64 iflags)
{
    int64 res, flags;
    res = s0;
    flags = iflags;
    EXECSHIFT("b", res, s1, 0, flags);
    /* overflow is undefined if count != 1 */
    if (s1 != 1)
      flags &= ~CC_O;
    xxprintf("%-10s A=%016llx B=%016llx R=%016llx CCIN=%04llx CC=%04llx\n",
           stringify(OP) "b", s0, s1, res, iflags, flags & CC_MASK);
}
#endif

void exec_op(int64 s2, int64 s0, int64 s1)
{
  int64 o,s,z,a,c,p,flags_in;
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
 
    exec_opq(s2, s0, s1, flags_in);
    if (s1 <= 31) 
       exec_opl(s2, s0, s1, flags_in);
#ifdef OP_SHIFTD
    if (s1 <= 15)
        exec_opw(s2, s0, s1, flags_in);
#else
    exec_opw(s2, s0, s1, flags_in);
#endif
#ifndef OP_NOBYTE
    exec_opb(s0, s1, flags_in);
#endif
#ifdef OP_CC
    exec_opq(s2, s0, s1, flags_in);
    exec_opl(s2, s0, s1, flags_in);
    exec_opw(s2, s0, s1, flags_in);
    exec_opb(s0, s1, flags_in);
#endif

  }}}}}}

}

void glue(test_, OP)(void)
{
    int64 i;
    for(i = 0; i < 64; i++)
        exec_op(0x3141592721ad3d34, 0x2718284612345678, i);
    for(i = 0; i < 64; i++)
        exec_op(0x31415927813f3421, 0x2718284682345678, i);
}

#undef OP
#undef OP_CC
#undef OP_SHIFTD
#undef OP_NOBYTE
#undef EXECSHIFT

