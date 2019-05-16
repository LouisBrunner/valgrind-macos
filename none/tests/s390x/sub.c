#include "sub.h"
#include "opcodes.h"

#define s(x, y) "s " x ", " y "\n"
#define sh(x, y) "sh " x ", " y "\n"
#define sg(x, y) "sg " x ", " y "\n"
#define sgf(x, y) "sgf " x ", " y "\n"
#define sl(x, y) "sl " x ", " y "\n"
#define slg(x, y) "slg " x ", " y "\n"
#define sgf(x, y) "sgf " x ", " y "\n"
#define slgf(x, y) "slgf " x ", " y "\n"
#define sr(x, y) "sr " x ", " y "\n"
#define sgr(x, y) "sgr " x ", " y "\n"
#define sgfr(x, y) "sgfr " x ", " y "\n"
#define slr(x, y) "slr " x ", " y "\n"
#define slgr(x, y) "slgr " x ", " y "\n"
#define slgfr(x, y) "slgfr " x ", " y "\n"
#define slb(x, y) "slb " x ", " y "\n"
#define slbg(x, y) "slbg " x ", " y "\n"
#define slbr(x, y) "slbr " x ", " y "\n"
#define slbgr(x, y) "slbgr " x ", " y "\n"

static void do_regmem_insns(unsigned long s2)
{
	memsweep(s, s2, 0);
	memsweep(sh, s2, 0);
	memsweep(sg, s2, 0);
	memsweep(sgf, s2, 0);
	memsweep(sl, s2, 0);
	memsweep(slg, s2, 0);
	memsweep(sgf, s2, 0);
	memsweep(slgf, s2, 0);
	regsweep(sr, s2, 0);
	regsweep(sgr, s2, 0);
	regsweep(sgfr, s2, 0);
	regsweep(slr, s2, 0);
	regsweep(slgr, s2, 0);
	regsweep(slgfr, s2, 0);
	memsweep(slb, s2, 0);
	memsweep(slbg, s2, 0);
	regsweep(slbr, s2, 0);
	regsweep(slbgr, s2, 0);
	memsweep(slb, s2, 1);
	memsweep(slbg, s2, 1);
	regsweep(slbr, s2, 1);
	regsweep(slbgr, s2, 1);
	ldispsweep(SHY, s2, 0);
	ldispsweep(SLY, s2, 0);
	ldispsweep(SY, s2, 0);
}

int main()
{
	for_each_m2(do_regmem_insns);
}
