
void glue(glue(test_, OP), b)(int64 op0, int64 op1) 
{
    int64 res, s1, s0, flags;
    s0 = op0;
    s1 = op1;
    res = s0;
    flags = 0;
    asm ("pushq %4\n\t"
         "popfq\n\t"
         stringify(OP)"b %b2\n\t" 
         "pushfq\n\t"
         "popq %1\n\t"
         : "=a" (res), "=g" (flags)
         : "q" (s1), "0" (res), "1" (flags));
    xxprintf("%-10s A=%016llx B=%016llx R=%016llx CC=%04llx\n",
           stringify(OP) "b", s0, s1, res, flags & CC_MASK);
}

void glue(glue(test_, OP), w)(int64 op0h, int64 op0, int64 op1) 
{
    int64 res, s1, flags, resh;
    s1 = op1;
    resh = op0h;
    res = op0;
    flags = 0;
    asm ("pushq %5\n\t"
         "popfq\n\t"
         stringify(OP) "w %w3\n\t" 
         "pushfq\n\t"
         "popq %1\n\t"
         : "=a" (res), "=g" (flags), "=d" (resh)
         : "q" (s1), "0" (res), "1" (flags), "2" (resh));
    xxprintf("%-10s AH=%016llx AL=%016llx B=%016llx RH=%016llx RL=%016llx CC=%04llx\n",
           stringify(OP) "w", op0h, op0, s1, resh, res, flags & CC_MASK);
}

void glue(glue(test_, OP), l)(int64 op0h, int64 op0, int64 op1) 
{
    int64 res, s1, flags, resh;
    s1 = op1;
    resh = op0h;
    res = op0;
    flags = 0;
    asm ("pushq %5\n\t"
         "popfq\n\t"
         stringify(OP) "l %3\n\t" 
         "pushfq\n\t"
         "popq %1\n\t"
         : "=a" (res), "=g" (flags), "=d" (resh)
         : "q" ((int)s1), "0" (res), "1" (flags), "2" (resh));
    xxprintf("%-10s AH=%016llx AL=%016llx B=%016llx RH=%016llx RL=%016llx CC=%04llx\n",
           stringify(OP) "l", op0h, op0, s1, resh, res, flags & CC_MASK);
}

void glue(glue(test_, OP), q)(int64 op0h, int64 op0, int64 op1) 
{
    int64 res, s1, flags, resh;
    s1 = op1;
    resh = op0h;
    res = op0;
    flags = 0;
    asm ("pushq %5\n\t"
         "popfq\n\t"
         stringify(OP) "q %3\n\t" 
         "pushfq\n\t"
         "popq %1\n\t"
         : "=a" (res), "=g" (flags), "=d" (resh)
         : "q" (s1), "0" (res), "1" (flags), "2" (resh));
    xxprintf("%-10s AH=%016llx AL=%016llx B=%016llx RH=%016llx RL=%016llx CC=%04llx\n",
           stringify(OP) "q", op0h, op0, s1, resh, res, flags & CC_MASK);
}

#undef OP
