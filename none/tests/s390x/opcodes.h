#ifndef OPCODES_H
#define OPCODES_H

/* Macro definitions for opcodes that are not generally available and may
   require extra facilities to be installed. These macros expand into a
   character string suitable for inclusion in an extended ASM statement
   in GCC. The macros generate .short and .long directives to encode the
   instruction. Those directives are available in all versions of binutils.

   Opcode macros have as many arguments as the opcode has variable fields.
   The fields appear in the argument list in the same order as in the opcode
   definition from left to right.

   The values to be encoded in those fields must be integer values in
   hexadecimal notation without a leading 0x. E.g. FLOGR(3,4) or 
   NIHF(3,00001234)
*/

#define RIL_RI(op1,r1,op2,i2)  \
            ".short 0x" #op1 #r1 #op2 "\n\t"  \
            ".long  0x" #i2 "\n\t"
#define RIE_RRI0(op1,r1,r3,i2,u0,op2)  \
            ".short 0x" #op1 #r1 #r3 "\n\t"  \
            ".long  0x" #i2 #u0 #op2 "\n\t"
#define RRF_R0RR2(op,r3,u0,r1,r2)  ".long 0x" #op #r3 #u0 #r1 #r2 "\n\t"
#define SIY_IRD(op1,i2,b1,dl1,dh1,op2)  \
            ".short 0x" #op1 #i2 "\n\t"  \
            ".long  0x" #b1 #dl1 #dh1 #op2 "\n\t"
#define RXY_RRRD(op1,r1,x2,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #x2 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RIL_RU(op1,r1,op2,i2)  \
            ".short 0x" #op1 #r1 #op2 "\n\t"  \
            ".long  0x" #i2 "\n\t"
#define RIL_RP(op1,r1,op2,i2)  \
            ".short 0x" #op1 #r1 #op2 "\n\t"  \
            ".long  0x" #i2 "\n\t"
#define SIL_RDI(op,b1,d1,i2)  \
            ".short 0x" #op "\n\t"  \
            ".long  0x" #b1 #d1 #i2 "\n\t"
#define RIS_RURDI(op1,r1,m3,b4,d4,i2,op2)  \
            ".short 0x" #op1 #r1 #m3 "\n\t"  \
            ".long  0x" #b4 #d4 #i2 #op2 "\n\t"
#define RIE_RUPI(op1,r1,m3,i4,i2,op2)  \
            ".short 0x" #op1 #r1 #m3 "\n\t"  \
            ".long  0x" #i4 #i2 #op2 "\n\t"
#define RRS(op1,r1,r2,b4,d4,m3,u0,op2)  \
            ".short 0x" #op1 #r1 #r2 "\n\t"  \
            ".long  0x" #b4 #d4 #m3 #u0 #op2 "\n\t"
#define RIE_RRPU(op1,r1,r2,i4,m3,u0,op2)  \
            ".short 0x" #op1 #r1 #r2 "\n\t"  \
            ".long  0x" #i4 #m3 #u0 #op2 "\n\t"
#define RRE_RR(op,u0,r1,r2)  ".long 0x" #op #u0 #r1 #r2 "\n\t"
#define RRE_RERE(op,r1,r2)  ".long 0x" #op "00" #r1 #r2 "\n\t"
#define RRE_R0(op,r1)       ".long 0x" #op "00" #r1 "0" "\n\t"
#define SIL_RDU(op,b1,d1,i2)  \
            ".short 0x" #op "\n\t"  \
            ".long  0x" #b1 #d1 #i2 "\n\t"
#define RIS_RURDU(op1,r1,m3,b4,d4,i2,op2)  \
            ".short 0x" #op1 #r1 #m3 "\n\t"  \
            ".long  0x" #b4 #d4 #i2 #op2 "\n\t"
#define RIE_RUPU(op1,r1,m3,i4,i2,op2)  \
            ".short 0x" #op1 #r1 #m3 "\n\t"  \
            ".long  0x" #i4 #i2 #op2 "\n\t"
#define SIY_URD(op1,i2,b1,dl1,dh1,op2)  \
            ".short 0x" #op1 #i2 "\n\t"  \
            ".long  0x" #b1 #dl1 #dh1 #op2 "\n\t"
#define RSY_RURD(op1,r1,r3,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #r3 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RRF_F0FF2(op,r3,u0,r1,r2)  ".long 0x" #op #r3 #u0 #r1 #r2 "\n\t"
#define RRF_FUFF2(op,r3,m4,r1,r2) ".long 0x" #op #r3 #m4 #r1 #r2 "\n\t"
#define RRF_UUFR(op,m3,m4,r1,r2)  ".long 0x" #op #m3 #m4 #r1 #r2 "\n\t"
#define RRF_UURF(op,m3,m4,r1,r2)  ".long 0x" #op #m3 #m4 #r1 #r2 "\n\t"

#define RSY_RRRD(op1,r1,r3,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #r3 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RSY_AARD(op1,r1,r3,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #r3 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RRE_FF(op,u0,r1,r2)  ".long 0x" #op #u0 #r1 #r2 "\n\t"
#define RRE_FR(op,u0,r1,r2)  ".long 0x" #op #u0 #r1 #r2 "\n\t"
#define RXY_FRRD(op1,r1,x2,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #x2 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RRE_RF(op,u0,r1,r2)  ".long 0x" #op #u0 #r1 #r2 "\n\t"
#define RSY_RDRM(op1,r1,r3,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #r3 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RRF_U0RR(op,r3,u0,r1,r2)  ".long 0x" #op #r3 #u0 #r1 #r2 "\n\t"
#define RXY_URRD(op1,r1,x2,b2,dl2,dh2,op2)  \
            ".short 0x" #op1 #r1 #x2 "\n\t"  \
            ".long  0x" #b2 #dl2 #dh2 #op2 "\n\t"
#define RIL_UP(op1,r1,op2,i2)  \
            ".short 0x" #op1 #r1 #op2 "\n\t"  \
            ".long  0x" #i2 "\n\t"
#define RIE_RRUUU(op1,r1,r2,i3,i4,i5,op2)  \
            ".short 0x" #op1 #r1 #r2 "\n\t"  \
            ".long  0x" #i3 #i4 #i5 #op2 "\n\t"
#define RRF_M0RERE(op,m3,r1,r2)  ".long 0x" #op #m3 "0" #r1 #r2 "\n\t"
#define S_RD(op,b2,d2) ".long 0x" #op #b2 #d2 "\n\t"

#define ADTRA(r3,m4,r1,r2)              RRF_FUFF2(b3d2,r3,m4,r1,r2)
#define AFI(r1,i2)                      RIL_RI(c2,r1,9,i2)
#define AGFI(r1,i2)                     RIL_RI(c2,r1,8,i2)
#define AGHIK(r1,r3,i2)                 RIE_RRI0(ec,r1,r3,i2,00,d9)
#define AGRK(r3,r1,r2)                  RRF_R0RR2(b9e8,r3,0,r1,r2)
#define AGSI(i2,b1,dl1,dh1)             SIY_IRD(eb,i2,b1,dl1,dh1,7a)
#define AHHHR(r3,r1,r2)                 RRF_R0RR2(b9c8,r3,0,r1,r2)
#define AHHLR(r3,r1,r2)                 RRF_R0RR2(b9d8,r3,0,r1,r2)
#define AHIK(r1,r3,i2)                  RIE_RRI0(ec,r1,r3,i2,00,d8)
#define AHY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,7a)
#define AIH(r1,i2)                      RIL_RI(cc,r1,8,i2)
#define ALFI(r1,i2)                     RIL_RU(c2,r1,b,i2)
#define ALGFI(r1,i2)                    RIL_RU(c2,r1,a,i2)
#define ALGHSIK(r1,r3,i2)               RIE_RRI0(ec,r1,r3,i2,00,db)
#define ALGRK(r3,r1,r2)                 RRF_R0RR2(b9ea,r3,0,r1,r2)
#define ALGSI(i2,b1,dl1,dh1)            SIY_IRD(eb,i2,b1,dl1,dh1,7e)
#define ALHHHR(r3,r1,r2)                RRF_R0RR2(b9ca,r3,0,r1,r2)
#define ALHHLR(r3,r1,r2)                RRF_R0RR2(b9da,r3,0,r1,r2)
#define ALHSIK(r1,r3,i2)                RIE_RRI0(ec,r1,r3,i2,00,da)
#define ALRK(r3,r1,r2)                  RRF_R0RR2(b9fa,r3,0,r1,r2)
#define ALSI(i2,b1,dl1,dh1)             SIY_IRD(eb,i2,b1,dl1,dh1,6e)
#define ALSIH(r1,i2)                    RIL_RI(cc,r1,a,i2)
#define ALSIHN(r1,i2)                   RIL_RI(cc,r1,b,i2)
#define ALY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,5e)
#define ARK(r3,r1,r2)                   RRF_R0RR2(b9f8,r3,0,r1,r2)
#define ASI(i2,b1,dl1,dh1)              SIY_IRD(eb,i2,b1,dl1,dh1,6a)
#define AXTRA(r3,m4,r1,r2)              RRF_FUFF2(b3da,r3,m4,r1,r2)
#define AY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,5a)
#define CDFBRA(m3,m4,r1,r2)             RRF_UUFR(b395,m3,m4,r1,r2)
#define CDFTR(m3,m4,r1,r2)              RRF_UUFR(b951,m3,m4,r1,r2)
#define CDGBRA(m3,m4,r1,r2)             RRF_UUFR(b3a5,m3,m4,r1,r2)
#define CDGTRA(m3,m4,r1,r2)             RRF_UUFR(b3f1,m3,m4,r1,r2)
#define CDLFBR(m3,m4,r1,r2)             RRF_UUFR(b391,m3,m4,r1,r2)
#define CDLFTR(m3,m4,r1,r2)             RRF_UUFR(b953,m3,m4,r1,r2)
#define CDLGBR(m3,m4,r1,r2)             RRF_UUFR(b3a1,m3,m4,r1,r2)
#define CDLGTR(m3,m4,r1,r2)             RRF_UUFR(b952,m3,m4,r1,r2)
#define CDTR(r1,r2)                     RRE_RERE(b3e4,r1,r2)
#define CEDTR(r1,r2)                    RRE_RERE(b3f4,r1,r2)
#define CEFBRA(m3,m4,r1,r2)             RRF_UUFR(b394,m3,m4,r1,r2)
#define CEGBRA(m3,m4,r1,r2)             RRF_UUFR(b3a4,m3,m4,r1,r2)
#define CELFBR(m3,m4,r1,r2)             RRF_UUFR(b390,m3,m4,r1,r2)
#define CELGBR(m3,m4,r1,r2)             RRF_UUFR(b3a0,m3,m4,r1,r2)
#define CEXTR(r1,r2)                    RRE_RERE(b3fc,r1,r2)
#define CFEBRA(m3,m4,r1,r2)             RRF_UURF(b398,m3,m4,r1,r2)
#define CFDBRA(m3,m4,r1,r2)             RRF_UURF(b399,m3,m4,r1,r2)
#define CFDTR(m3,m4,r1,r2)              RRF_UURF(b941,m3,m4,r1,r2)
#define CFI(r1,i2)                      RIL_RI(c2,r1,d,i2)
#define CFXBRA(m3,m4,r1,r2)             RRF_UURF(b39a,m3,m4,r1,r2)
#define CFXTR(m3,m4,r1,r2)              RRF_UURF(b949,m3,m4,r1,r2)
#define CGDBRA(m3,m4,r1,r2)             RRF_UURF(b3a9,m3,m4,r1,r2)
#define CGDTRA(m3,m4,r1,r2)             RRF_UURF(b3e1,m3,m4,r1,r2)
#define CGEBRA(m3,m4,r1,r2)             RRF_UURF(b3a8,m3,m4,r1,r2)
#define CGFI(r1,i2)                     RIL_RI(c2,r1,c,i2)
#define CGFRL(r1,i2)                    RIL_RP(c6,r1,c,i2)
#define CGH(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,34)
#define CGHRL(r1,i2)                    RIL_RP(c6,r1,4,i2)
#define CGHSI(b1,d1,i2)                 SIL_RDI(e558,b1,d1,i2)
#define CGIB(r1,m3,b4,d4,i2)            RIS_RURDI(ec,r1,m3,b4,d4,i2,fc)
#define CGIJ(r1,m3,i4,i2)               RIE_RUPI(ec,r1,m3,i4,i2,7c)
#define CGRB(r1,r2,b4,d4,m3)            RRS(ec,r1,r2,b4,d4,m3,0,e4)
#define CGRJ(r1,r2,i4,m3)               RIE_RRPU(ec,r1,r2,i4,m3,0,64)
#define CGRL(r1,i2)                     RIL_RP(c6,r1,8,i2)
#define CGXBRA(m3,m4,r1,r2)             RRF_UURF(b3aa,m3,m4,r1,r2)
#define CGXTRA(m3,m4,r1,r2)             RRF_UURF(b3e9,m3,m4,r1,r2)
#define CHF(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,cd)
#define CHHR(r1,r2)                     RRE_RR(b9cd,00,r1,r2)
#define CHHSI(b1,d1,i2)                 SIL_RDI(e554,b1,d1,i2)
#define CHLR(r1,r2)                     RRE_RR(b9dd,00,r1,r2)
#define CHRL(r1,i2)                     RIL_RP(c6,r1,5,i2)
#define CHSI(b1,d1,i2)                  SIL_RDI(e55c,b1,d1,i2)
#define CHY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,79)
#define CIB(r1,m3,b4,d4,i2)             RIS_RURDI(ec,r1,m3,b4,d4,i2,fe)
#define CIH(r1,i2)                      RIL_RI(cc,r1,d,i2)
#define CIJ(r1,m3,i4,i2)                RIE_RUPI(ec,r1,m3,i4,i2,7e)
#define CLFEBR(m3,m4,r1,r2)             RRF_UURF(b39c,m3,m4,r1,r2)
#define CLFDBR(m3,m4,r1,r2)             RRF_UURF(b39d,m3,m4,r1,r2)
#define CLFDTR(m3,m4,r1,r2)             RRF_UURF(b943,m3,m4,r1,r2)
#define CLFXBR(m3,m4,r1,r2)             RRF_UURF(b39e,m3,m4,r1,r2)
#define CLFXTR(m3,m4,r1,r2)             RRF_UURF(b94b,m3,m4,r1,r2)
#define CLFHSI(b1,d1,i2)                SIL_RDU(e55d,b1,d1,i2)
#define CLFI(r1,i2)                     RIL_RU(c2,r1,f,i2)
#define CLGDBR(m3,m4,r1,r2)             RRF_UURF(b3ad,m3,m4,r1,r2)
#define CLGDTR(m3,m4,r1,r2)             RRF_UURF(b942,m3,m4,r1,r2)
#define CLGEBR(m3,m4,r1,r2)             RRF_UURF(b3ac,m3,m4,r1,r2)
#define CLGFI(r1,i2)                    RIL_RU(c2,r1,e,i2)
#define CLGFRL(r1,i2)                   RIL_RP(c6,r1,e,i2)
#define CLGHRL(r1,i2)                   RIL_RP(c6,r1,6,i2)
#define CLGHSI(b1,d1,i2)                SIL_RDU(e559,b1,d1,i2)
#define CLGIB(r1,m3,b4,d4,i2)           RIS_RURDU(ec,r1,m3,b4,d4,i2,fd)
#define CLGIJ(r1,m3,i4,i2)              RIE_RUPU(ec,r1,m3,i4,i2,7d)
#define CLGRB(r1,r2,b4,d4,m3)           RRS(ec,r1,r2,b4,d4,m3,0,e5)
#define CLGRJ(r1,r2,i4,m3)              RIE_RRPU(ec,r1,r2,i4,m3,0,65)
#define CLGRL(r1,i2)                    RIL_RP(c6,r1,a,i2)
#define CLGXBR(m3,m4,r1,r2)             RRF_UURF(b3ae,m3,m4,r1,r2)
#define CLGXTR(m3,m4,r1,r2)             RRF_UURF(b94a,m3,m4,r1,r2)
#define CLHF(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,cf)
#define CLHHR(r1,r2)                    RRE_RR(b9cf,00,r1,r2)
#define CLHHSI(b1,d1,i2)                SIL_RDU(e555,b1,d1,i2)
#define CLHLR(r1,r2)                    RRE_RR(b9df,00,r1,r2)
#define CLHRL(r1,i2)                    RIL_RP(c6,r1,7,i2)
#define CLIB(r1,m3,b4,d4,i2)            RIS_RURDU(ec,r1,m3,b4,d4,i2,ff)
#define CLIH(r1,i2)                     RIL_RU(cc,r1,f,i2)
#define CLIJ(r1,m3,i4,i2)               RIE_RUPU(ec,r1,m3,i4,i2,7f)
#define CLIY(i2,b1,dl1,dh1)             SIY_URD(eb,i2,b1,dl1,dh1,55)
#define CLMY(r1,r3,b2,dl2,dh2)          RSY_RURD(eb,r1,r3,b2,dl2,dh2,21)
#define CLRB(r1,r2,b4,d4,m3)            RRS(ec,r1,r2,b4,d4,m3,0,f7)
#define CLRJ(r1,r2,i4,m3)               RIE_RRPU(ec,r1,r2,i4,m3,0,77)
#define CLRL(r1,i2)                     RIL_RP(c6,r1,f,i2)
#define CLY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,55)
#define CPSDR(r3,r1,r2)                 RRF_F0FF2(b372,r3,0,r1,r2)
#define CRB(r1,r2,b4,d4,m3)             RRS(ec,r1,r2,b4,d4,m3,0,f6)
#define CRJ(r1,r2,i4,m3)                RIE_RRPU(ec,r1,r2,i4,m3,0,76)
#define CRL(r1,i2)                      RIL_RP(c6,r1,d,i2)
#define CSY(r1,r3,b2,dl2,dh2)           RSY_RRRD(eb,r1,r3,b2,dl2,dh2,14)
#define CU12(m3,r1,r2)                  RRF_M0RERE(b2a7,m3,r1,r2)
#define CU14(m3,r1,r2)                  RRF_M0RERE(b9b0,m3,r1,r2)
#define CU21(m3,r1,r2)                  RRF_M0RERE(b2a6,m3,r1,r2)
#define CU24(m3,r1,r2)                  RRF_M0RERE(b9b1,m3,r1,r2)
#define CU41(r1,r2)                     RRE_RERE(b9b2,r1,r2)
#define CU42(r1,r2)                     RRE_RERE(b9b3,r1,r2)
#define CVBY(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,06)
#define CVDY(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,26)
#define CXFBRA(m3,m4,r1,r2)             RRF_UUFR(b396,m3,m4,r1,r2)
#define CXFTR(m3,m4,r1,r2)              RRF_UUFR(b959,m3,m4,r1,r2)
#define CXGBRA(m3,m4,r1,r2)             RRF_UUFR(b3a6,m3,m4,r1,r2)
#define CXGTRA(m3,m4,r1,r2)             RRF_UUFR(b3f9,m3,m4,r1,r2)
#define CXLFBR(m3,m4,r1,r2)             RRF_UUFR(b392,m3,m4,r1,r2)
#define CXLFTR(m3,m4,r1,r2)             RRF_UUFR(b95b,m3,m4,r1,r2)
#define CXLGBR(m3,m4,r1,r2)             RRF_UUFR(b3a2,m3,m4,r1,r2)
#define CXLGTR(m3,m4,r1,r2)             RRF_UUFR(b95a,m3,m4,r1,r2)
#define CXTR(r1,r2)                     RRE_RERE(b3ec,r1,r2)
#define CY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,59)
#define DDTRA(r3,m4,r1,r2)              RRF_FUFF2(b3d1,r3,m4,r1,r2)
#define DXTRA(r3,m4,r1,r2)              RRF_FUFF2(b3d9,r3,m4,r1,r2)
#define EXRL(r1,i2)                     RIL_RP(c6,r1,0,i2)
#define FLOGR(r1,r2)                    RRE_RR(b983,00,r1,r2)
#define ICMY(r1,r3,b2,dl2,dh2)          RSY_RURD(eb,r1,r3,b2,dl2,dh2,81)
#define ICY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,73)
#define IIHF(r1,i2)                     RIL_RU(c0,r1,8,i2)
#define IILF(r1,i2)                     RIL_RU(c0,r1,9,i2)
#define LAA(r1,r3,b2,dl2,dh2)           RSY_RRRD(eb,r1,r3,b2,dl2,dh2,f8)
#define LAAG(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,e8)
#define LAAL(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,fa)
#define LAALG(r1,r3,b2,dl2,dh2)         RSY_RRRD(eb,r1,r3,b2,dl2,dh2,ea)
#define LAEY(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,75)
#define LAMY(r1,r3,b2,dl2,dh2)          RSY_AARD(eb,r1,r3,b2,dl2,dh2,9a)
#define LAN(r1,r3,b2,dl2,dh2)           RSY_RRRD(eb,r1,r3,b2,dl2,dh2,f4)
#define LANG(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,e4)
#define LAO(r1,r3,b2,dl2,dh2)           RSY_RRRD(eb,r1,r3,b2,dl2,dh2,f6)
#define LAOG(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,e6)
#define LAX(r1,r3,b2,dl2,dh2)           RSY_RRRD(eb,r1,r3,b2,dl2,dh2,f7)
#define LAXG(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,e7)
#define LAY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,71)
#define LB(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,76)
#define LBH(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,c0)
#define LBR(r1,r2)                      RRE_RR(b926,00,r1,r2)
#define LCDFR(r1,r2)                    RRE_FF(b373,00,r1,r2)
#define LDGR(r1,r2)                     RRE_FR(b3c1,00,r1,r2)
#define LDY(r1,x2,b2,dl2,dh2)           RXY_FRRD(ed,r1,x2,b2,dl2,dh2,65)
#define LEY(r1,x2,b2,dl2,dh2)           RXY_FRRD(ed,r1,x2,b2,dl2,dh2,64)
#define LFH(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,ca)
#define LGB(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,77)
#define LGBR(r1,r2)                     RRE_RR(b906,00,r1,r2)
#define LGDR(r1,r2)                     RRE_RF(b3cd,00,r1,r2)
#define LGFI(r1,i2)                     RIL_RI(c0,r1,1,i2)
#define LGFRL(r1,i2)                    RIL_RP(c4,r1,c,i2)
#define LGHR(r1,r2)                     RRE_RR(b907,00,r1,r2)
#define LGHRL(r1,i2)                    RIL_RP(c4,r1,4,i2)
#define LGRL(r1,i2)                     RIL_RP(c4,r1,8,i2)
#define LHH(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,c4)
#define LHR(r1,r2)                      RRE_RR(b927,00,r1,r2)
#define LHRL(r1,i2)                     RIL_RP(c4,r1,5,i2)
#define LHY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,78)
#define LLC(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,94)
#define LLCH(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,c2)
#define LLCR(r1,r2)                     RRE_RR(b994,00,r1,r2)
#define LLGCR(r1,r2)                    RRE_RR(b984,00,r1,r2)
#define LLGFRL(r1,i2)                   RIL_RP(c4,r1,e,i2)
#define LLGHR(r1,r2)                    RRE_RR(b985,00,r1,r2)
#define LLGHRL(r1,i2)                   RIL_RP(c4,r1,6,i2)
#define LLH(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,95)
#define LLHH(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,c6)
#define LLHR(r1,r2)                     RRE_RR(b995,00,r1,r2)
#define LLHRL(r1,i2)                    RIL_RP(c4,r1,2,i2)
#define LLIHF(r1,i2)                    RIL_RU(c0,r1,e,i2)
#define LLILF(r1,i2)                    RIL_RU(c0,r1,f,i2)
#define LMY(r1,r3,b2,dl2,dh2)           RSY_RRRD(eb,r1,r3,b2,dl2,dh2,98)
#define LNDFR(r1,r2)                    RRE_FF(b371,00,r1,r2)
#define LOC(r1,r3,b2,dl2,dh2)           RSY_RDRM(eb,r1,r3,b2,dl2,dh2,f2)
#define LOCG(r1,r3,b2,dl2,dh2)          RSY_RDRM(eb,r1,r3,b2,dl2,dh2,e2)
#define LOCGR(r3,r1,r2)                 RRF_U0RR(b9e2,r3,0,r1,r2)
#define LOCR(r3,r1,r2)                  RRF_U0RR(b9f2,r3,0,r1,r2)
#define LPDFR(r1,r2)                    RRE_FF(b370,00,r1,r2)
#define LRL(r1,i2)                      RIL_RP(c4,r1,d,i2)
#define LT(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,12)
#define LTG(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,02)
#define LTGF(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,32)
#define LY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,58)
#define MDTRA(r3,m4,r1,r2)              RRF_FUFF2(b3d0,r3,m4,r1,r2)
#define MFY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,5c)
#define MHY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,7c)
#define MSFI(r1,i2)                     RIL_RI(c2,r1,1,i2)
#define MSGFI(r1,i2)                    RIL_RI(c2,r1,0,i2)
#define MSY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,51)
#define MVGHI(b1,d1,i2)                 SIL_RDI(e548,b1,d1,i2)
#define MVHHI(b1,d1,i2)                 SIL_RDI(e544,b1,d1,i2)
#define MVHI(b1,d1,i2)                  SIL_RDI(e54c,b1,d1,i2)
#define MVIY(i2,b1,dl1,dh1)             SIY_URD(eb,i2,b1,dl1,dh1,52)
#define MXTRA(r3,m4,r1,r2)              RRF_FUFF2(b3d8,r3,m4,r1,r2)
#define NGRK(r3,r1,r2)                  RRF_R0RR2(b9e4,r3,0,r1,r2)
#define NIHF(r1,i2)                     RIL_RU(c0,r1,a,i2)
#define NILF(r1,i2)                     RIL_RU(c0,r1,b,i2)
#define NIY(i2,b1,dl1,dh1)              SIY_URD(eb,i2,b1,dl1,dh1,54)
#define NRK(r3,r1,r2)                   RRF_R0RR2(b9f4,r3,0,r1,r2)
#define NY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,54)
#define OGRK(r3,r1,r2)                  RRF_R0RR2(b9e6,r3,0,r1,r2)
#define OIHF(r1,i2)                     RIL_RU(c0,r1,c,i2)
#define OILF(r1,i2)                     RIL_RU(c0,r1,d,i2)
#define OIY(i2,b1,dl1,dh1)              SIY_URD(eb,i2,b1,dl1,dh1,56)
#define ORK(r3,r1,r2)                   RRF_R0RR2(b9f6,r3,0,r1,r2)
#define OY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,56)
#define PFD(r1,x2,b2,dl2,dh2)           RXY_URRD(e3,r1,x2,b2,dl2,dh2,36)
#define PFDRL(r1,i2)                    RIL_UP(c6,r1,2,i2)
#define RISBG(r1,r2,i3,i4,i5)           RIE_RRUUU(ec,r1,r2,i3,i4,i5,55)
#define RNSBG(r1,r2,i3,i4,i5)           RIE_RRUUU(ec,r1,r2,i3,i4,i5,54)
#define ROSBG(r1,r2,i3,i4,i5)           RIE_RRUUU(ec,r1,r2,i3,i4,i5,56)
#define RXSBG(r1,r2,i3,i4,i5)           RIE_RRUUU(ec,r1,r2,i3,i4,i5,57)
#define SFPC(r1)                        RRE_R0(b384,r1)
#define SGRK(r3,r1,r2)                  RRF_R0RR2(b9e9,r3,0,r1,r2)
#define SHHHR(r3,r1,r2)                 RRF_R0RR2(b9c9,r3,0,r1,r2)
#define SHHLR(r3,r1,r2)                 RRF_R0RR2(b9d9,r3,0,r1,r2)
#define SHY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,7b)
#define SLAK(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,dd)
#define ECAG(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,4c)
#define SDTRA(r3,m4,r1,r2)              RRF_FUFF2(b3d3,r3,m4,r1,r2)
#define SLFI(r1,i2)                     RIL_RU(c2,r1,5,i2)
#define SLGFI(r1,i2)                    RIL_RU(c2,r1,4,i2)
#define SLGRK(r3,r1,r2)                 RRF_R0RR2(b9eb,r3,0,r1,r2)
#define SLHHHR(r3,r1,r2)                RRF_R0RR2(b9cb,r3,0,r1,r2)
#define SLHHLR(r3,r1,r2)                RRF_R0RR2(b9db,r3,0,r1,r2)
#define SLLK(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,df)
#define SLRK(r3,r1,r2)                  RRF_R0RR2(b9fb,r3,0,r1,r2)
#define SLY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,5f)
#define SRAK(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,dc)
#define SRK(r3,r1,r2)                   RRF_R0RR2(b9f9,r3,0,r1,r2)
#define SRLK(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,de)
#define SRNMB(b2,d2)                    S_RD(b2b8,b2,d2)
#define SRNMT(b2,d2)                    S_RD(b2b9,b2,d2)
#define STAMY(r1,r3,b2,dl2,dh2)         RSY_AARD(eb,r1,r3,b2,dl2,dh2,9b)
#define STCH(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,c3)
#define STCMY(r1,r3,b2,dl2,dh2)         RSY_RURD(eb,r1,r3,b2,dl2,dh2,2d)
#define STCY(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,72)
#define STDY(r1,x2,b2,dl2,dh2)          RXY_FRRD(ed,r1,x2,b2,dl2,dh2,67)
#define STEY(r1,x2,b2,dl2,dh2)          RXY_FRRD(ed,r1,x2,b2,dl2,dh2,66)
#define STFH(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,cb)
#define STGRL(r1,i2)                    RIL_RP(c4,r1,b,i2)
#define STHH(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,c7)
#define STHRL(r1,i2)                    RIL_RP(c4,r1,7,i2)
#define STHY(r1,x2,b2,dl2,dh2)          RXY_RRRD(e3,r1,x2,b2,dl2,dh2,70)
#define STMY(r1,r3,b2,dl2,dh2)          RSY_RRRD(eb,r1,r3,b2,dl2,dh2,90)
#define STOC(r1,r3,b2,dl2,dh2)          RSY_RDRM(eb,r1,r3,b2,dl2,dh2,f3)
#define STOCG(r1,r3,b2,dl2,dh2)         RSY_RDRM(eb,r1,r3,b2,dl2,dh2,e3)
#define STRL(r1,i2)                     RIL_RP(c4,r1,f,i2)
#define STY(r1,x2,b2,dl2,dh2)           RXY_RRRD(e3,r1,x2,b2,dl2,dh2,50)
#define SXTRA(r3,m4,r1,r2)              RRF_FUFF2(b3db,r3,m4,r1,r2)
#define SY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,5b)
#define TMY(i2,b1,dl1,dh1)              SIY_URD(eb,i2,b1,dl1,dh1,51)
#define XGRK(r3,r1,r2)                  RRF_R0RR2(b9e7,r3,0,r1,r2)
#define XIHF(r1,i2)                     RIL_RU(c0,r1,6,i2)
#define XILF(r1,i2)                     RIL_RU(c0,r1,7,i2)
#define XIY(i2,b1,dl1,dh1)              SIY_URD(eb,i2,b1,dl1,dh1,57)
#define XRK(r3,r1,r2)                   RRF_R0RR2(b9f7,r3,0,r1,r2)
#define XY(r1,x2,b2,dl2,dh2)            RXY_RRRD(e3,r1,x2,b2,dl2,dh2,57)

#endif /* OPCODES_H */
