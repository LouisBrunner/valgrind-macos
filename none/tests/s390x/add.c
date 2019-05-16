#include "add.h"
#include "opcodes.h"

#define ahi(x, y) "ahi " x ", " y "\n"
#define aghi(x, y) "aghi " x ", " y "\n"

static void do_imm_insns(void)
{
	immsweep(ahi, 0, 0);
	immsweep(ahi, -1, 0);
	immsweep(ahi, -32768, 0);
	immsweep(ahi, 32767, 0);
	immsweep(aghi, 0, 0);
	immsweep(aghi, -1, 0);
	immsweep(aghi, -32768, 0);
	immsweep(aghi, 32767, 0);
}

#define a(x, y) "a " x ", " y "\n"
#define ah(x, y) "ah " x ", " y "\n"
#define ag(x, y) "ag " x ", " y "\n"
#define agf(x, y) "agf " x ", " y "\n"
#define al(x, y) "al " x ", " y "\n"
#define alg(x, y) "alg " x ", " y "\n"
#define agf(x, y) "agf " x ", " y "\n"
#define algf(x, y) "algf " x ", " y "\n"
#define ar(x, y) "ar " x ", " y "\n"
#define agr(x, y) "agr " x ", " y "\n"
#define agfr(x, y) "agfr " x ", " y "\n"
#define alr(x, y) "alr " x ", " y "\n"
#define algr(x, y) "algr " x ", " y "\n"
#define algfr(x, y) "algfr " x ", " y "\n"
#define alc(x, y) "alc " x ", " y "\n"
#define alcg(x, y) "alcg " x ", " y "\n"
#define alcr(x, y) "alcr " x ", " y "\n"
#define alcgr(x, y) "alcgr " x ", " y "\n"

static void do_regmem_insns(unsigned long s2)
{
	memsweep(a, s2, 0);
	memsweep(ah, s2, 0);
	memsweep(ag, s2, 0);
	memsweep(agf, s2, 0);
	memsweep(al, s2, 0);
	memsweep(alg, s2, 0);
	memsweep(agf, s2, 0);
	memsweep(algf, s2, 0);
	regsweep(ar, s2, 0);
	regsweep(agr, s2, 0);
	regsweep(agfr, s2, 0);
	regsweep(alr, s2, 0);
	regsweep(algr, s2, 0);
	regsweep(algfr, s2, 0);
	memsweep(alc, s2, 0);
	memsweep(alcg, s2, 0);
	regsweep(alcr, s2, 0);
	regsweep(alcgr, s2, 0);
	memsweep(alc, s2, 1);
	memsweep(alcg, s2, 1);
	regsweep(alcr, s2, 1);
	regsweep(alcgr, s2, 1);
	ldispsweep(AHY, s2, 0);
	ldispsweep(AY, s2, 0);
	ldispsweep(ALY, s2, 0);
}

int main()
{
	for_each_m2(do_regmem_insns);

	do_imm_insns();
}
