
/*  Copyright (C) 2006 Dave Nomura
       dcnltc@us.ibm.com

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307, USA.

    The GNU General Public License is contained in the file COPYING.
*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef enum { FALSE=0, TRUE } bool_t;

typedef enum {
	FADDS, FSUBS, FMULS, FDIVS,
	FMADDS, FMSUBS, FNMADDS, FNMSUBS,
	FADD, FSUB, FMUL, FDIV, FMADD,
	FMSUB, FNMADD, FNMSUB, FSQRT
} flt_op_t;

typedef enum {
	TO_NEAREST=0, TO_ZERO, TO_PLUS_INFINITY, TO_MINUS_INFINITY } round_mode_t;
char *round_mode_name[] = { "near", "zero", "+inf", "-inf" };

const char *flt_op_names[] = {
	"fadds", "fsubs", "fmuls", "fdivs",
	"fmadds", "fmsubs", "fnmadds", "fnmsubs",
	"fadd", "fsub", "fmul", "fdiv", "fmadd", "fmsub", "fnmadd",
	"fnmsub", "fsqrt"
};

typedef unsigned int fpscr_t;

typedef union {
	float flt;
	struct {
		unsigned int sign:1;
		unsigned int exp:8;
		unsigned int frac:23;
	} layout;
} flt_overlay;

typedef union {
	double dbl;
	struct {
		unsigned int sign:1;
		unsigned int exp:11;
		unsigned int frac_hi:20;
		unsigned int frac_lo:32;
	} layout;
	struct {
		unsigned int hi;
		unsigned int lo;
	} dbl_pair;
} dbl_overlay;

void assert_fail(const char *msg,
	const char* expr, const char* file, int line, const char*fn);

#define STRING(__str)  #__str
#define assert(msg, expr)                                           \
  ((void) ((expr) ? 0 :                                         \
           (assert_fail (msg, STRING(expr),                  \
                             __FILE__, __LINE__,                \
                             __PRETTY_FUNCTION__), 0)))
float denorm_small;
double dbl_denorm_small;
float norm_small;
bool_t debug = FALSE;
bool_t long_is_64_bits = sizeof(long) == 8;

void assert_fail (msg, expr, file, line, fn)
const char* msg;
const char* expr;
const char* file;
int line;
const char*fn;
{
   printf( "\n%s: %s:%d (%s): Assertion `%s' failed.\n",
               msg, file, line, fn, expr );
   exit( 1 );
}
void set_rounding_mode(round_mode_t mode)
{
	switch(mode) {
	case TO_NEAREST:
		asm volatile("mtfsfi 7, 0");
		break;
	case TO_ZERO:
		asm volatile("mtfsfi 7, 1");
		break;
	case TO_PLUS_INFINITY:
		asm volatile("mtfsfi 7, 2");
		break;
	case TO_MINUS_INFINITY:
		asm volatile("mtfsfi 7, 3");
		break;
	}
}

void print_double(char *msg, double dbl)
{
	dbl_overlay D;
	D.dbl = dbl;

	printf("%15s : dbl %-20a = %c(%4d, %05x%08x)\n",
			msg, D.dbl, (D.layout.sign == 0 ? '+' : '-'),
			D.layout.exp, D.layout.frac_hi, D.layout.frac_lo);
}

void print_single(char *msg, float *flt)
{
	flt_overlay F;
	F.flt = *flt;

	/* NOTE: for the purposes of comparing the fraction of a single with
	**       a double left shift the .frac so that hex digits are grouped
	**	     from left to right.  this is necessary because the size of a 
	**		 single mantissa (23) bits is not a multiple of 4
	*/
	printf("%15s : flt %-20a = %c(%4d, %06x)\n",
		msg, F.flt, (F.layout.sign == 0 ? '+' : '-'), F.layout.exp, F.layout.frac << 1);
}

int check_dbl_to_flt_round(round_mode_t mode, double dbl, float *expected)
{
	int status = 0;
	flt_overlay R, E;
	char *result;
	char *eq_ne;

	set_rounding_mode(mode);

	E.flt = *expected;
	R.flt = (float)dbl;

	if ((R.layout.sign != E.layout.sign) ||
		(R.layout.exp != E.layout.exp) ||
		(R.layout.frac != E.layout.frac)) {
		result = "FAILED";
		eq_ne = "!=";
		status = 1;
	} else {
		result = "PASSED";
		eq_ne = "==";
		status = 0;
	}
	printf("%s:%s:(double)(%-20a) = %20a",
		round_mode_name[mode], result, R.flt, dbl);
	if (status) {
		print_single("\n\texpected", &E.flt);
		print_single("\n\trounded ", &R.flt);
	}
	putchar('\n');
	return status;
}

int test_dbl_to_float_convert(char *msg, float *base)
{
	int status = 0;
	double half = (double)denorm_small/2;
	double qtr = half/2;
	double D_hi = (double)*base + half + qtr;
	double D_lo = (double)*base + half - qtr;
	float F_lo = *base;
	float F_hi = F_lo + denorm_small;


	/*
	** .....+-----+-----+-----+-----+---....
	**      ^F_lo ^           ^     ^
	**            D_lo
	**                        D_hi
	**                              F_hi
	** F_lo and F_hi are two consecutive single float model numbers
	** denorm_small distance apart. D_lo and D_hi are two numbers
	** within that range that are not representable as single floats
	** and will be rounded to either F_lo or F_hi.
	*/
	printf("-------------------------- %s --------------------------\n", msg);
	if (debug) {
		print_double("D_lo", D_lo);
		print_double("D_hi", D_hi);
		print_single("F_lo", &F_lo);
		print_single("F_hi", &F_hi);
	}

	/* round to nearest */
	status |= check_dbl_to_flt_round(TO_NEAREST, D_hi, &F_hi);
	status |= check_dbl_to_flt_round(TO_NEAREST, D_lo, &F_lo);

	/* round to zero */
	status |= check_dbl_to_flt_round(TO_ZERO, D_hi, (D_hi > 0 ? &F_lo : &F_hi));
	status |= check_dbl_to_flt_round(TO_ZERO, D_lo, (D_hi > 0 ? &F_lo : &F_hi));

	/* round to +inf */
	status |= check_dbl_to_flt_round(TO_PLUS_INFINITY, D_hi, &F_hi);
	status |= check_dbl_to_flt_round(TO_PLUS_INFINITY, D_lo, &F_hi);

	/* round to -inf */
	status |= check_dbl_to_flt_round(TO_MINUS_INFINITY, D_hi, &F_lo);
	status |= check_dbl_to_flt_round(TO_MINUS_INFINITY, D_lo, &F_lo);
	return status;
}

void
init()
{
	flt_overlay F;
	dbl_overlay D;

	/* small is the smallest denormalized single float number */
	F.layout.sign = 0;
	F.layout.exp = 0;
	F.layout.frac = 1;
	denorm_small = F.flt;	/* == 2^(-149) */
	if (debug) {
		print_double("float small", F.flt);
	}

	D.layout.sign = 0;
	D.layout.exp = 0;
	D.layout.frac_hi = 0;
	D.layout.frac_lo = 1;
	dbl_denorm_small = D.dbl;	/* == 2^(-1022) */
	if (debug) {
		print_double("double small", D.dbl);
	}

	/* n_small is the smallest normalized single precision float */
	F.layout.exp = 1;
	norm_small = F.flt;
}

int check_int_to_flt_round(round_mode_t mode, long L, float *expected)
{
	int status = 0;
	int I = L;
	char *int_name = "int";
	flt_overlay R, E;
	char *result;
	int iter;

	set_rounding_mode(mode);
	E.flt = *expected;

	for (iter = 0; iter < 2; iter++) {
		int stat = 0;
		R.flt = (iter == 0 ? (float)I : (float)L);

		if ((R.layout.sign != E.layout.sign) ||
			(R.layout.exp != E.layout.exp) ||
			(R.layout.frac != E.layout.frac)) {
			result = "FAILED";
			stat = 1;
		} else {
			result = "PASSED";
			stat = 0;
		}
		printf("%s:%s:(float)(%4s)%9d = %11.1f",
			round_mode_name[mode], result, int_name, I, R.flt);
		if (stat) {
			print_single("\n\texpected: %.1f ", &E.flt);
			print_single("\n\trounded ", &R.flt);
		}
		putchar('\n');
		status |= stat;

		if (!long_is_64_bits) break;
		int_name = "long";
	}
	return status;
}

int check_long_to_dbl_round(round_mode_t mode, long L, double *expected)
{
	int status = 0;
	dbl_overlay R, E;
	char *result;

	set_rounding_mode(mode);
	E.dbl = *expected;

	R.dbl = (double)L;

	if ((R.layout.sign != E.layout.sign) ||
		(R.layout.exp != E.layout.exp) ||
		(R.layout.frac_lo != E.layout.frac_lo) ||
		(R.layout.frac_hi != E.layout.frac_hi)) {
		result = "FAILED";
		status = 1;
	} else {
		result = "PASSED";
		status = 0;
	}
	printf("%s:%s:(double)(%18ld) = %20.1f",
		round_mode_name[mode], result, L, R.dbl);
	if (status) {
		printf("\n\texpected %.1f : ", E.dbl);
	}
	putchar('\n');
	return status;
}

int test_int_to_float_convert(char *msg)
{
	int status = 0;
	int int24_hi = 0x03ff0fff;
	int int24_lo = 0x03ff0ffd;
	float pos_flt_lo = 67047420.0;
	float pos_flt_hi = 67047424.0;
	float neg_flt_lo = -67047420.0;
	float neg_flt_hi = -67047424.0;

	printf("-------------------------- %s --------------------------\n", msg);
	status |= check_int_to_flt_round(TO_NEAREST, int24_lo, &pos_flt_lo);
	status |= check_int_to_flt_round(TO_NEAREST, int24_hi, &pos_flt_hi);
	status |= check_int_to_flt_round(TO_ZERO, int24_lo, &pos_flt_lo);
	status |= check_int_to_flt_round(TO_ZERO, int24_hi, &pos_flt_lo);
	status |= check_int_to_flt_round(TO_PLUS_INFINITY, int24_lo, &pos_flt_hi);
	status |= check_int_to_flt_round(TO_PLUS_INFINITY, int24_hi, &pos_flt_hi);
	status |= check_int_to_flt_round(TO_MINUS_INFINITY, int24_lo, &pos_flt_lo);
	status |= check_int_to_flt_round(TO_MINUS_INFINITY, int24_hi, &pos_flt_lo);

	status |= check_int_to_flt_round(TO_NEAREST, -int24_lo, &neg_flt_lo);
	status |= check_int_to_flt_round(TO_NEAREST, -int24_hi, &neg_flt_hi);
	status |= check_int_to_flt_round(TO_ZERO, -int24_lo, &neg_flt_lo);
	status |= check_int_to_flt_round(TO_ZERO, -int24_hi, &neg_flt_lo);
	status |= check_int_to_flt_round(TO_PLUS_INFINITY, -int24_lo, &neg_flt_lo);
	status |= check_int_to_flt_round(TO_PLUS_INFINITY, -int24_hi, &neg_flt_lo);
	status |= check_int_to_flt_round(TO_MINUS_INFINITY, -int24_lo, &neg_flt_hi);
	status |= check_int_to_flt_round(TO_MINUS_INFINITY, -int24_hi, &neg_flt_hi);
	return status;
}

#ifdef __powerpc64__
int test_long_to_double_convert(char *msg)
{
	int status = 0;
	long long55_hi = 0x07ff0ffffffffff;
	long long55_lo = 0x07ff0fffffffffd;
	double pos_dbl_lo = 36012304344547324.0;
	double pos_dbl_hi = 36012304344547328.0;
	double neg_dbl_lo = -36012304344547324.0;
	double neg_dbl_hi = -36012304344547328.0;

	printf("-------------------------- %s --------------------------\n", msg);
	status |= check_long_to_dbl_round(TO_NEAREST, long55_lo, &pos_dbl_lo);
	status |= check_long_to_dbl_round(TO_NEAREST, long55_hi, &pos_dbl_hi);
	status |= check_long_to_dbl_round(TO_ZERO, long55_lo, &pos_dbl_lo);
	status |= check_long_to_dbl_round(TO_ZERO, long55_hi, &pos_dbl_lo);
	status |= check_long_to_dbl_round(TO_PLUS_INFINITY, long55_lo, &pos_dbl_hi);
	status |= check_long_to_dbl_round(TO_PLUS_INFINITY, long55_hi, &pos_dbl_hi);
	status |= check_long_to_dbl_round(TO_MINUS_INFINITY, long55_lo, &pos_dbl_lo);
	status |= check_long_to_dbl_round(TO_MINUS_INFINITY, long55_hi, &pos_dbl_lo);

	status |= check_long_to_dbl_round(TO_NEAREST, -long55_lo, &neg_dbl_lo);
	status |= check_long_to_dbl_round(TO_NEAREST, -long55_hi, &neg_dbl_hi);
	status |= check_long_to_dbl_round(TO_ZERO, -long55_lo, &neg_dbl_lo);
	status |= check_long_to_dbl_round(TO_ZERO, -long55_hi, &neg_dbl_lo);
	status |= check_long_to_dbl_round(TO_PLUS_INFINITY, -long55_lo, &neg_dbl_lo);
	status |= check_long_to_dbl_round(TO_PLUS_INFINITY, -long55_hi, &neg_dbl_lo);
	status |= check_long_to_dbl_round(TO_MINUS_INFINITY, -long55_lo, &neg_dbl_hi);
	status |= check_long_to_dbl_round(TO_MINUS_INFINITY, -long55_hi, &neg_dbl_hi);
	return status;
}
#endif

int check_single_arithmetic_op(flt_op_t op)
{
		char *result;
        int status = 0;
        dbl_overlay R, E;
        double qtr, half, fA, fB, fD;
		round_mode_t mode;
		int q, s;
		bool_t two_args = TRUE;
		float whole = denorm_small;

#define BINOP(op) \
        __asm__ volatile( \
					op" %0, %1, %2\n\t" \
					: "=f"(fD) : "f"(fA) , "f"(fB));
#define UNOP(op) \
        __asm__ volatile( \
					op" %0, %1\n\t" \
					: "=f"(fD) : "f"(fA));

		half = (double)whole/2;
		qtr = half/2;

		if (debug) {
			print_double("qtr", qtr);
			print_double("whole", whole);
			print_double("2*whole", 2*whole);
		}

		for (mode = TO_NEAREST; mode <= TO_MINUS_INFINITY; mode++)
		for (s = -1; s < 2; s += 2)
		for (q = 1; q < 4; q += 2) {
			double expected;
			double lo = s*whole;
			double hi = s*2*whole;

			switch(op) {
			case FADDS:
				fA = s*whole;
				fB = s*q*qtr;
				break;
			case FSUBS:
				fA = s*2*whole;
				fB = s*(q == 1 ? 3 : 1)*qtr;
				break;
			case FMULS:
				fA = 0.5;
				fB = s*(4+q)*half;
				break;
			case FDIVS:
				fA = s*(4+q)*half;
				fB = 2.0;
				break;
			default:
				assert("check_single_arithmetic_op: unexpected op",
					FALSE);
				break;
			}

			switch(mode) {
			case TO_NEAREST:
				expected = (q == 1 ? lo : hi);
				break;
			case TO_ZERO:
				expected = lo;
				break;
			case TO_PLUS_INFINITY:
				expected = (s == 1 ? hi : lo);
				break;
			case TO_MINUS_INFINITY:
				expected = (s == 1 ? lo : hi);
				break;
			}
		
			set_rounding_mode(mode);

			/*
			** do the double precision dual operation just for comparison
			** when debugging
			*/
			switch(op) {
			case FADDS:
				BINOP("fadds");
				R.dbl = fD;
				BINOP("fadd");
				break;
			case FSUBS:
				BINOP("fsubs");
				R.dbl = fD;
				BINOP("fsub");
				break;
			case FMULS:
				BINOP("fmuls");
				R.dbl = fD;
				BINOP("fmul");
				break;
			case FDIVS:
				BINOP("fdivs");
				R.dbl = fD;
				BINOP("fdiv");
				break;
			default:
				assert("check_single_arithmetic_op: unexpected op",
					FALSE);
				break;
			}
#undef UNOP
#undef BINOP

			E.dbl = expected;

			if ((R.layout.sign != E.layout.sign) ||
				(R.layout.exp != E.layout.exp) ||
				(R.layout.frac_lo != E.layout.frac_lo) ||
				(R.layout.frac_hi != E.layout.frac_hi)) {
				result = "FAILED";
				status = 1;
			} else {
				result = "PASSED";
				status = 0;
			}

			printf("%s:%s:%s(%-13a",
				round_mode_name[mode], result, flt_op_names[op], fA);
			if (two_args) printf(", %-13a", fB);
			printf(") = %-13a", R.dbl);
			if (status) printf("\n\texpected %a", E.dbl);
			putchar('\n');

			if (debug) {
				print_double("hi", hi);
				print_double("lo", lo);
				print_double("expected", expected);
				print_double("got", R.dbl);
				print_double("double result", fD);
			}
		}

		return status;
}

int check_single_guarded_arithmetic_op(flt_op_t op)
{
		typedef struct {
			int num, den, frac;
		} fdivs_t;

		char *result;
        int status = 0;
        flt_overlay A, B, Z;
        dbl_overlay Res, Exp;
        double fA, fB, fC, fD;
		round_mode_t mode;
		int g, s;
		int arg_count;

		fdivs_t divs_guard_cases[16] = {
			{ 105, 56, 0x700000 },  /* : 0 */
			{ 100, 57, 0x608FB8 },  /* : 1 */
			{ 000, 00, 0x000000 },  /* : X */
			{ 100, 52, 0x762762 },  /* : 3 */
			{ 000, 00, 0x000000 },  /* : X */
			{ 100, 55, 0x68BA2E },  /* : 5 */
			{ 000, 00, 0x000000 },  /* : X */
			{ 100, 51, 0x7AFAFA },  /* : 7 */
			{ 000, 00, 0x000000 },  /* : X */
			{ 100, 56, 0x649249 },  /* : 9 */
			{ 000, 00, 0x000000 },  /* : X */
			{ 100, 54, 0x6D097B },  /* : B */
			{ 000, 00, 0x000000 },  /* : X */
			{ 100, 59, 0x58F2FB },  /* : D */
			{ 000, 00, 0x000000 },  /* : X */
			{ 101, 52, 0x789D89 }  /* : F */
		};

		/*	0x1.00000 00000000p-3 */
		/* set up the invariant fields of B, the arg to cause rounding */
		B.flt = 0.0;
		B.layout.exp = 124;  /* -3 */

		/* set up args so result is always Z = 1.200000000000<g>p+0 */
		Z.flt = 1.0;
		Z.layout.sign = 0;

#define TERNOP(op) \
		arg_count = 3; \
        __asm__ volatile( \
					op" %0, %1, %2, %3\n\t" \
					: "=f"(fD) : "f"(fA) , "f"(fB), "f"(fC));
#define BINOP(op) \
		arg_count = 2; \
        __asm__ volatile( \
					op" %0, %1, %2\n\t" \
					: "=f"(fD) : "f"(fA) , "f"(fB));
#define UNOP(op) \
		arg_count = 1; \
        __asm__ volatile( \
					op" %0, %1\n\t" \
					: "=f"(fD) : "f"(fA));

	for (mode = TO_NEAREST; mode <= TO_MINUS_INFINITY; mode++)
	for (s = -1; s < 2; s += 2)
	for (g = 0; g < 16; g += 1) {
		double lo, hi, expected;
		int LSB;
		int guard = 0;
		int z_sign = s;

		/*
		** one argument will have exponent = 0 as will the result (by
		** design) so choose the other argument with exponent -3 to
		** force a 3 bit shift for scaling leaving us with 3 guard bits
		** and the LSB bit at the bottom of the manitssa.
		*/
		switch(op) {
		case FADDS:
			/* 1p+0 + 1.00000<g>p-3 */
			B.layout.frac = g;

			fB = s*B.flt;
			fA = s*1.0;

			/* set up Z to be truncated result */

			/* mask off LSB from resulting guard bits */
			guard = g & 7;

			Z.layout.frac = 0x100000 | (g >> 3);
			break;
		case FSUBS:
			/* 1.200002p+0 - 1.000000000000<g>p-3 */
			A.flt = 1.125;
			/* add enough to avoid scaling of the result */
			A.layout.frac |= 0x2;
			fA = s*A.flt;

			B.layout.frac = g;
			fB = s*B.flt;

			/* set up Z to be truncated result */
			guard = (0x10-g);
			Z.layout.frac = guard>>3;

			/* mask off LSB from resulting guard bits */
			guard &= 7;
			break;
		case FMULS:
			/* 1 + g*2^-23 */
			A.flt = 1.0;
			A.layout.frac = g;
			fA = s*A.flt;
			fB = 1.125;

			/* set up Z to be truncated result */
			Z.flt = 1.0;
			Z.layout.frac = 0x100000;
			Z.layout.frac |= g + (g>>3);
			guard = g & 7;
			break;
		case FDIVS:
			/* g >> 3 == LSB, g & 7 == guard bits */
			guard = g & 7;
			if ((guard & 1) == 0) {
				/* special case: guard bit X = 0 */
				A.flt = denorm_small;
				A.layout.frac = g;
				fA = A.flt;
				fB = s*8.0;
				Z.flt = 0.0;
				Z.layout.frac |= (g >> 3);
			} else {
				fA = s*divs_guard_cases[g].num;
				fB = divs_guard_cases[g].den;

				Z.flt = 1.0;
				Z.layout.frac = divs_guard_cases[g].frac;
			}
			break;
		case FMADDS:
		case FMSUBS:
		case FNMADDS:
		case FNMSUBS:
			/* 1 + g*2^-23 */
			A.flt = 1.0;
			A.layout.frac = g;
			fA = s*A.flt;
			fB = 1.125;

			/* 1.000001p-1 */
			A.flt = 0.5;
			A.layout.frac = 1;
			fC = (op == FMADDS || op == FNMADDS ? s : -s)*A.flt;

			/* set up Z to be truncated result */
			z_sign = (op == FNMADDS || op == FNMSUBS ? -s : s);
			guard = ((g & 7) + 0x4) & 7;
			Z.flt = 1.0;
			Z.layout.frac = 0x500000;
			Z.layout.frac |= g + (g>>3) + ((g & 7)>> 2 ? 1 : 0);
			break;
		default:
			assert("check_single_arithmetic_op: unexpected op",
				FALSE);
			break;
		}

		/* get LSB for tie breaking */
		LSB = Z.layout.frac & 1;

		/* set up hi and lo */
		lo = z_sign*Z.flt;
		Z.layout.frac += 1;
		hi = z_sign*Z.flt;

		switch(mode) {
		case TO_NEAREST:
			/* look at 3 guard bits to determine expected rounding */
			switch(guard) {
			case 0:
			case 1: case 2: case 3:
				expected = lo;
				break;
			case 4:	/* tie: round to even */
				if (debug) printf("tie: LSB = %d\n", LSB);
				expected = (LSB == 0 ? lo : hi);
				break;
			case 5: case 6: case 7:
				expected = hi;
				break;
			default:
				assert("check_single_guarded_arithmetic_op: unexpected guard",
					FALSE);
			}
			break;
		case TO_ZERO:
			expected = lo;
			break;
		case TO_PLUS_INFINITY:
			if (guard == 0) {
				/* no rounding */
				expected = lo;
			} else {
				expected = (s == 1 ? hi : lo);
			}
			break;
		case TO_MINUS_INFINITY:
			if (guard == 0) {
				/* no rounding */
				expected = lo;
			} else {
				expected = (s == 1 ? lo : hi);
			}
			break;
		}
		
		set_rounding_mode(mode);

		/*
		** do the double precision dual operation just for comparison
		** when debugging
		*/
		switch(op) {
		case FADDS:
			BINOP("fadds");
			Res.dbl = fD;
			break;
		case FSUBS:
			BINOP("fsubs");
			Res.dbl = fD;
			break;
		case FMULS:
			BINOP("fmuls");
			Res.dbl = fD;
			break;
		case FDIVS:
			BINOP("fdivs");
			Res.dbl = fD;
			break;
		case FMADDS:
			TERNOP("fmadds");
			Res.dbl = fD;
			break;
		case FMSUBS:
			TERNOP("fmsubs");
			Res.dbl = fD;
			break;
		case FNMADDS:
			TERNOP("fnmadds");
			Res.dbl = fD;
			break;
		case FNMSUBS:
			TERNOP("fnmsubs");
			Res.dbl = fD;
			break;
		default:
			assert("check_single_guarded_arithmetic_op: unexpected op",
				FALSE);
			break;
		}
#undef UNOP
#undef BINOP
#undef TERNOP

		Exp.dbl = expected;

		if ((Res.layout.sign != Exp.layout.sign) ||
			(Res.layout.exp != Exp.layout.exp) ||
			(Res.layout.frac_lo != Exp.layout.frac_lo) ||
			(Res.layout.frac_hi != Exp.layout.frac_hi)) {
			result = "FAILED";
			status = 1;
		} else {
			result = "PASSED";
			status = 0;
		}

		printf("%s:%s:%s(%-13f",
			round_mode_name[mode], result, flt_op_names[op], fA);
		if (arg_count > 1) printf(", %-13a", fB);
		if (arg_count > 2) printf(", %-13a", fC);
		printf(") = %-13a", Res.dbl);
		if (status) printf("\n\texpected %a", Exp.dbl);
		putchar('\n');

		if (debug) {
			print_double("hi", hi);
			print_double("lo", lo);
			print_double("expected", expected);
			print_double("got", Res.dbl);
		}
	}

	return status;
}

int check_double_guarded_arithmetic_op(flt_op_t op)
{
	typedef struct {
		int num, den, hi, lo;
	} fdiv_t;
	typedef struct {
		double arg;
		int exp, hi, lo;
	} fsqrt_t;

	char *result;
	int status = 0;
	dbl_overlay A, B, Z;
	dbl_overlay Res, Exp;
	double fA, fB, fC, fD;
	round_mode_t mode;
	int g, s;
	int arg_count;
	fdiv_t div_guard_cases[16] = {
		{ 62, 62, 0x00000, 0x00000000 },	/* 0 */
		{ 64, 62, 0x08421, 0x08421084 },	/* 1 */
		{ 66, 62, 0x10842, 0x10842108 },	/* 2 */
		{ 100, 62, 0x9ce73, 0x9ce739ce },	/* 3 */
		{ 100, 62, 0x9ce73, 0x9ce739ce },	/* X */
		{ 102, 62, 0xa5294, 0xa5294a52 },	/* 5 */
		{ 106, 62, 0xb5ad6, 0xb5ad6b5a },	/* 6 */
		{ 108, 62, 0xbdef7, 0xbdef7bde },	/* 7 */
		{ 108, 108, 0x00000, 0x00000000 },	/* 8 */
		{ 112, 62, 0xce739, 0xce739ce7 },	/* 9 */
		{ 114, 62, 0xd6b5a, 0xd6b5ad6b },	/* A */
		{ 116, 62, 0xdef7b, 0xdef7bdef },	/* B */
		{ 84, 62, 0x5ad6b, 0x5ad6b5ad },	/* X */
		{ 118, 62, 0xe739c, 0xe739ce73 },	/* D */
		{ 90, 62, 0x739ce, 0x739ce739 },	/* E */
		{ 92, 62, 0x7bdef, 0x7bdef7bd }		/* F */
	};


	fsqrt_t sqrt_guard_cases[16] = {
		{ 0x1.08800p0,  0, 0x04371, 0xd9ab72fb}, /* :0 B8.8440  */ 
		{ 0x0.D2200p0, -1, 0xcfdca, 0xf353049e}, /* :1 A4.6910  */
		{ 0x1.A8220p0,  0, 0x49830, 0x2b49cd6d}, /* :2 E9.D411  */ 
		{ 0x1.05A20p0,  0, 0x02cd1, 0x3b44f3bf}, /* :3 B7.82D1  */
		{ 0x0.CA820p0, -1, 0xc7607, 0x3cec0937}, /* :4 A1.6541  */ 
		{ 0x1.DCA20p0,  0, 0x5d4f8, 0xd4e4c2b2}, /* :5 F7.EE51  */
		{ 0x1.02C80p0,  0, 0x01630, 0x9cde7483}, /* :6 B6.8164  */ 
		{ 0x0.DC800p0, -1, 0xdb2cf, 0xe686fe7c}, /* :7 A8.6E40  */
		{ 0x0.CF920p0, -1, 0xcd089, 0xb6860626}, /* :8 A3.67C9  */ 
		{ 0x1.1D020p0,  0, 0x0e1d6, 0x2e78ed9d}, /* :9 BF.8E81  */
		{ 0x0.E1C80p0, -1, 0xe0d52, 0x6020fb6b}, /* :A AA.70E4  */ 
		{ 0x0.C8000p0, -1, 0xc48c6, 0x001f0abf}, /* :B A0.6400  */
		{ 0x1.48520p0,  0, 0x21e9e, 0xd813e2e2}, /* :C CD.A429  */ 
		{ 0x0.F4C20p0, -1, 0xf4a1b, 0x09bbf0b0}, /* :D B1.7A61  */
		{ 0x0.CD080p0, -1, 0xca348, 0x79b907ae}, /* :E A2.6684  */ 
		{ 0x1.76B20p0,  0, 0x35b67, 0x81aed827}  /* :F DB.BB59  */
	};

	/*	0x1.00000 00000000p-3 */
	/* set up the invariant fields of B, the arg to cause rounding */
	B.dbl = 0.0;
	B.layout.exp = 1020;

	/* set up args so result is always Z = 1.200000000000<g>p+0 */
	Z.dbl = 1.0;
	Z.layout.sign = 0;

#define TERNOP(op) \
		arg_count = 3; \
        __asm__ volatile( \
					op" %0, %1, %2, %3\n\t" \
					: "=f"(fD) : "f"(fA) , "f"(fB), "f"(fC));
#define BINOP(op) \
		arg_count = 2; \
        __asm__ volatile( \
					op" %0, %1, %2\n\t" \
					: "=f"(fD) : "f"(fA) , "f"(fB));
#define UNOP(op) \
		arg_count = 1; \
        __asm__ volatile( \
					op" %0, %1\n\t" \
					: "=f"(fD) : "f"(fA));

	for (mode = TO_NEAREST; mode <= TO_MINUS_INFINITY; mode++)
	for (s = (op != FSQRT ? -1 : 1); s < 2; s += 2)
	for (g = 0; g < 16; g += 1) {
		double lo, hi, expected;
		int LSB;
		int guard;
		int z_sign = s;

		/*
		** one argument will have exponent = 0 as will the result (by
		** design) so choose the other argument with exponent -3 to
		** force a 3 bit shift for scaling leaving us with 3 guard bits
		** and the LSB bit at the bottom of the manitssa.
		*/
		switch(op) {
		case FADD:
			/* 1p+0 + 1.000000000000<g>p-3 */
			B.layout.frac_lo = g;

			fB = s*B.dbl;
			fA = s*1.0;

			/* set up Z to be truncated result */

			/* mask off LSB from resulting guard bits */
			guard = g & 7;

			Z.layout.frac_hi = 0x20000;
			Z.layout.frac_lo = g >> 3;

			break;
		case FSUB:
			/* 1.2000000000002p+0 - 1.000000000000<g>p-3 */
			A.dbl = 1.125;
			/* add enough to avoid scaling of the result */
			A.layout.frac_lo = 0x2;
			fA = s*A.dbl;

			B.layout.frac_lo = g;
			fB = s*B.dbl;

			/* set up Z to be truncated result */
			guard = (0x10-g);
			Z.layout.frac_hi = 0x0;
			Z.layout.frac_lo = guard>>3;

			/* mask off LSB from resulting guard bits */
			guard &= 7;
			break;
		case FMUL:
			/* 1 + g*2^-52 */
			A.dbl = 1.0;
			A.layout.frac_lo = g;
			fA = s*A.dbl;
			fB = 1.125;

			/* set up Z to be truncated result */
			Z.dbl = 1.0;
			Z.layout.frac_hi = 0x20000;
			Z.layout.frac_lo = g + (g>>3);
			guard = g & 7;
			break;
		case FMADD:
		case FMSUB:
		case FNMADD:
		case FNMSUB:
			/* 1 + g*2^-52 */
			A.dbl = 1.0;
			A.layout.frac_lo = g;
			fA = s*A.dbl;
			fB = 1.125;

			/* 1.0000000000001p-1 */
			A.dbl = 0.5;
			A.layout.frac_lo = 1;
			fC = (op == FMADD || op == FNMADD ? s : -s)*A.dbl;

			/* set up Z to be truncated result */
			z_sign = (op == FNMADD || op == FNMSUB ? -s : s);
			guard = ((g & 7) + 0x4) & 7;
			Z.dbl = 1.0;
			Z.layout.frac_hi = 0xa0000;
			Z.layout.frac_lo = g + (g>>3) + ((g & 7)>> 2 ? 1 : 0);
			break;
		case FDIV:
			/* g >> 3 == LSB, g & 7 == guard bits */
			guard = g & 7;
			if (guard == 0x4) {
				/* special case guard bits == 4, inexact tie */
				fB = s*2.0;
				Z.dbl = 0.0;
				if (g >> 3) {
					fA = dbl_denorm_small + 2*dbl_denorm_small;
					Z.layout.frac_lo = 0x1;
				} else {
					fA = dbl_denorm_small;
				}
			} else {
				fA = s*div_guard_cases[g].num;
				fB = div_guard_cases[g].den;

				printf("%d/%d\n",
					s*div_guard_cases[g].num,
					div_guard_cases[g].den);
				Z.dbl = 1.0;
				Z.layout.frac_hi = div_guard_cases[g].hi;
				Z.layout.frac_lo = div_guard_cases[g].lo;
			}
			break;
		case FSQRT:
			fA = s*sqrt_guard_cases[g].arg;
			Z.dbl = 1.0;
			Z.layout.exp = sqrt_guard_cases[g].exp + 1023;
			Z.layout.frac_hi = sqrt_guard_cases[g].hi;
			Z.layout.frac_lo = sqrt_guard_cases[g].lo;
			guard = g >> 1;
			if (g & 1) guard |= 1;
			/* don't have test cases for when X bit = 0 */
			if (guard == 0 || guard == 4) continue;
			break;
		default:
			assert("check_double_guarded_arithmetic_op: unexpected op",
				FALSE);
			break;
		}

		/* get LSB for tie breaking */
		LSB = Z.layout.frac_lo & 1;

		/* set up hi and lo */
		lo = z_sign*Z.dbl;
		Z.layout.frac_lo += 1;
		hi = z_sign*Z.dbl;

		switch(mode) {
		case TO_NEAREST:
			/* look at 3 guard bits to determine expected rounding */
			switch(guard) {
			case 0:
			case 1: case 2: case 3:
				expected = lo;
				break;
			case 4:	/* tie: round to even */
				if (debug) printf("tie: LSB = %d\n", LSB);
				expected = (LSB == 0 ? lo : hi);
				break;
			case 5: case 6: case 7:
				expected = hi;
				break;
			default:
				assert("check_double_guarded_arithmetic_op: unexpected guard",
					FALSE);
			}
			break;
		case TO_ZERO:
			expected = lo;
			break;
		case TO_PLUS_INFINITY:
			if (guard == 0) {
				/* no rounding */
				expected = lo;
			} else {
				expected = (s == 1 ? hi : lo);
			}
			break;
		case TO_MINUS_INFINITY:
			if (guard == 0) {
				/* no rounding */
				expected = lo;
			} else {
				expected = (s == 1 ? lo : hi);
			}
			break;
		}
	
		set_rounding_mode(mode);

		/*
		** do the double precision dual operation just for comparison
		** when debugging
		*/
		switch(op) {
		case FADD:
			BINOP("fadd");
			Res.dbl = fD;
			break;
		case FSUB:
			BINOP("fsub");
			Res.dbl = fD;
			break;
		case FMUL:
			BINOP("fmul");
			Res.dbl = fD;
			break;
		case FMADD:
			TERNOP("fmadd");
			Res.dbl = fD;
			break;
		case FMSUB:
			TERNOP("fmsub");
			Res.dbl = fD;
			break;
		case FNMADD:
			TERNOP("fnmadd");
			Res.dbl = fD;
			break;
		case FNMSUB:
			TERNOP("fnmsub");
			Res.dbl = fD;
			break;
		case FDIV:
			BINOP("fdiv");
			Res.dbl = fD;
			break;
		case FSQRT:
			UNOP("fsqrt");
			Res.dbl = fD;
			break;
		default:
			assert("check_double_guarded_arithmetic_op: unexpected op",
				FALSE);
			break;
		}
#undef UNOP
#undef BINOP
#undef TERNOP

		Exp.dbl = expected;

		if ((Res.layout.sign != Exp.layout.sign) ||
			(Res.layout.exp != Exp.layout.exp) ||
			(Res.layout.frac_lo != Exp.layout.frac_lo) ||
			(Res.layout.frac_hi != Exp.layout.frac_hi)) {
			result = "FAILED";
			status = 1;
		} else {
			result = "PASSED";
			status = 0;
		}

		printf("%s:%s:%s(%-13a",
			round_mode_name[mode], result, flt_op_names[op], fA);
		if (arg_count > 1) printf(", %-13a", fB);
		if (arg_count > 2) printf(", %-13a", fC);
		printf(") = %-13a", Res.dbl);
		if (status) printf("\n\texpected %a", Exp.dbl);
		putchar('\n');

		if (debug) {
			print_double("hi", hi);
			print_double("lo", lo);
			print_double("expected", expected);
			print_double("got", Res.dbl);
		}
	}

	return status;
}

int test_float_arithmetic_ops()
{
	int status = 0;
	flt_op_t op;

	/*
	** choose FP operands whose result should be rounded to either
	** lo or hi.
	*/

	printf("-------------------------- %s --------------------------\n",
		"test rounding of float operators without guard bits");
	for (op = FADDS; op <= FDIVS; op++) {
		status |= check_single_arithmetic_op(op);
	}

	printf("-------------------------- %s --------------------------\n",
		"test rounding of float operators with guard bits");
	for (op = FADDS; op <= FNMSUBS; op++) {
		status |= check_single_guarded_arithmetic_op(op);
	}

	printf("-------------------------- %s --------------------------\n",
		"test rounding of double operators with guard bits");
	for (op = FADD; op <= FSQRT; op++) {
		status |= check_double_guarded_arithmetic_op(op);
	}
	return status;
}


int
main()
{
	int status = 0;

	init();

	status |= test_dbl_to_float_convert("test denormalized convert", &denorm_small);
	status |= test_dbl_to_float_convert("test normalized convert", &norm_small);
	status |= test_int_to_float_convert("test (float)int convert");
	status |= test_int_to_float_convert("test (float)int convert");

#ifdef __powerpc64__
	status |= test_long_to_double_convert("test (double)long convert");
#endif
	status |= test_float_arithmetic_ops();
	return status;
}
