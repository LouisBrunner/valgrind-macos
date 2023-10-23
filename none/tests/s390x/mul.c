#include "mul.h"
#include "opcodes.h"

#define mhi(x, y) "mhi " x ", " y "\n"
#define mghi(x, y) "mghi " x ", " y "\n"

static void do_imm_insns(void)
{
	immsweep(mhi, 0);
	immsweep(mhi, -1);
	immsweep(mhi, -32768);
	immsweep(mhi, 32767);
	immsweep(mghi, 0);
	immsweep(mghi, -1);
	immsweep(mghi, -32768);
	immsweep(mghi, 32767);
}

#define m(x, y) "m " x ", " y "\n"
#define mr(x, y) "mr " x ", " y "\n"
#define mh(x, y) "mh " x ", " y "\n"
#define mlg(x, y) "mlg " x ", " y "\n"
#define mlgr(x, y) "mlgr " x ", " y "\n"
#define ml(x, y) "ml " x ", " y "\n"
#define mlr(x, y) "mlr " x ", " y "\n"
#define ms(x, y) "ms " x ", " y "\n"
#define msr(x, y) "msr " x ", " y "\n"
#define msg(x, y) "msg " x ", " y "\n"
#define msgr(x, y) "msgr " x ", " y "\n"
#define msgf(x, y) "msgf " x ", " y "\n"
#define msgfr(x, y) "msgfr " x ", " y "\n"
#define msy(x, y) "msy " x ", " y "\n"

static void do_regmem_insns(unsigned long m2)
{
	rmemsweep(m, m2);
	regsweep(mr, m2);
	rmemsweep(mh, m2);
	memsweep(mlg, m2);
	regsweep(mlgr, m2);
	memsweep(ml, m2);
	regsweep(mlr, m2);
	rmemsweep(ms, m2);
	regsweep(msr, m2);
	memsweep(msg, m2);
	regsweep(msgr, m2);
	memsweep(msgf, m2);
	regsweep(msgfr, m2);
	memsweep(msy, m2);
}

int main()
{
	for_each_m2(do_regmem_insns);
	do_imm_insns();
}
