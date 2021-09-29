/*
 * Valgrind testcase for mcrxrx.
 * This is a power9 (isa 3.0) instruction that copies
 * OV,OV32,CA,CA32 fields from the XER and places them
 * into a specified field of the CR. */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

unsigned long long current_cr;
unsigned long long current_xer;

inline static void dissect_cr_field (unsigned long long full_cr, int offset) {
	int mask;
	int crfield;
	mask = 0xf << (offset*4);
	crfield = full_cr & mask;
	crfield = crfield >> 4*offset;

	if (crfield & 0x01) printf("(LT)"); else printf(" -  ");
	if (crfield & 0x02) printf("(GT)"); else printf(" -  ");
	if (crfield & 0x04) printf("(EQ)"); else printf(" -  ");
	if (crfield & 0x08) printf("(SO)"); else printf(" -  ");
}

/* dissect_xer helpers */
static char * xer_strings[] = {
	" 0-RSVD", " 1-RSVD", " 2-RSVD", " 3-RSVD", " 4-RSVD", " 5-RSVD", " 6-RSVD",
	" 7-RSVD", " 8-RSVD", " 9-RSVD", "10-RSVD", "11-RSVD", "12-RSVD", "13-RSVD",
	"14-RSVD", "15-RSVD", "16-RSVD", "17-RSVD", "18-RSVD", "19-RSVD",
	"20-RSVD", "21-RSVD", "22-RSVD", "23-RSVD", "24-RSVD", "25-RSVD",
	"26-RSVD", "27-RSVD", "28-RSVD", "29-RSVD", "30-RSVD", "31-RSVD",
	/* 32 */ "SO", "OV", "CA",
	/* 35 */ "35-RSVD", "36-RSVD", "37-RSVD", "38-RSVD", "39-RSVD",
	/* 40 */ "40-RSVD", "41-RSVD", "42-RSVD", "43-RSVD",
	/* 44 */ "OV32", "CA32",
	/* 46 */ "46-RSVD", "47-RSVD", "48-RSVD", "49-RSVD", "50-RSVD", "51-RSVD",
	"52-RSVD", "53-RSVD", "54-RSVD", "55-RSVD", "56-RSVD",
	/* 57:63 # bytes transferred by a Load/Store String Indexed instruction. */
	"LSI/SSI-0", "LSI/SSI-1", "LSI/SSI-2", "LSI/SSI-3",
	"LSI/SSI-4", "LSI/SSI-5", "LSI/SSI-6",
};

/* Dissect the XER register contents.
*/
static void dissect_xer_raw(unsigned long local_xer) {
	int i;
	long mybit;
        char mystr[30];
	strcpy(mystr,"");
	for (i = 0; i <= 63; i++) {
		mybit = 1ULL << (63 - i); /* compensate for reversed bit numbering. */
		if (mybit & local_xer) {
			strcat(mystr,xer_strings[i]);
			strcat(mystr," ");
		}
	}
   printf(" %16s",mystr);
}

int main (int argc, char **argv)
{
#if HAS_ISA_3_00
        /* init xer to zero. */
	unsigned long long Rx = 0;
	__asm__ __volatile__ ("mtxer %0" : : "r" (Rx) :"xer");

	/* iterate over each of the interesting fields in the xer
	 * OV,CA,OV32,CA32.   Build a field with those bits set, and
	 * push that into the XER (mtxer).    */
	unsigned long long i18,i19,i29,i30;
	for (i30=0; i30<2; i30++) { // iterate OV
		for (i29=0; i29<2; i29++) { // iterate CA
			for (i19=0; i19<2; i19++) { // iterate OV32
				for (i18=0; i18<2; i18++) { // iterate CA32
					Rx  = i18 << 18;
					Rx += i19 << 19;
					Rx += i29 << 29;
					Rx += i30 << 30;

					printf("mcrxrx ");

					/* move 'Rx' value into the xer. */
					__asm__ __volatile__ ("mtxer %0" : : "r" (Rx) :"xer");

					/* Retrieve the XER and print it. */
					__asm__ __volatile__ ("mfxer %0" : "=b"(current_xer) );

					/* Moving the xer contents to the CR field # 2
					 *  using the mcrxr instruction. */
					__asm__ __volatile__ (".machine push;" \
							      ".machine power9;" \
							      "mcrxrx 2;" \
							      ".machine pop;" );

					/* Copy the cr into a reg so we can print it.. */
					__asm__ __volatile__ ("mfcr %0"  : "=b"(current_cr) );

					dissect_xer_raw(current_xer);
					printf(" => ");
					dissect_cr_field(current_cr,5);
					printf("\n");
				}
			}
		}
	}
#else
        printf("HAS_ISA_3_00 not detected.\n");
#endif
	return 0;
}

