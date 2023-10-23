// This small program computes a Fast Fourier Transform.  It tests
// Valgrind's handling of FP operations.  It is representative of all
// programs that do a lot of FP operations.

// Licensing: This program is closely based on the one of the same name from
// http://www.fourmilab.ch/.  The front page of that site says:
//
//   "Except for a few clearly-marked exceptions, all the material on this
//   site is in the public domain and may be used in any manner without
//   permission, restriction, attribution, or compensation."

/*

	Two-dimensional FFT benchmark

	Designed and implemented by John Walker in April of 1989.

	This  benchmark executes a specified number of passes (default
	20) through a loop in which each  iteration  performs  a  fast
	Fourier transform of a square matrix (default size 256x256) of
	complex numbers (default precision double),  followed  by  the
	inverse  transform.   After  all loop iterations are performed
	the results are checked against known correct values.

	This benchmark is intended for use on C implementations  which
        define  "int"  as  32 bits or longer and permit allocation and
	direct addressing of arrays larger than one megabyte.

	If CAPOUT is defined,  the  result  after  all	iterations  is
	written  as  a	CA  Lab  pattern  file.   This is intended for
	debugging in case horribly wrong results  are  obtained  on  a
	given machine.

	Archival  timings  are	run  with the definitions below set as
	follows: Float = double, Asize = 256, Passes = 20, CAPOUT  not
	defined.

	Time (seconds)		    System

            2393.93       Sun 3/260, SunOS 3.4, C, "-f68881 -O".
			  (John Walker).

            1928          Macintosh IIx, MPW C 3.0, "-mc68020
                          -mc68881 -elems881 -m".  (Hugh Hoover).

            1636.1        Sun 4/110, "cc -O3 -lm".  (Michael McClary).
			  The suspicion is that this is software
			  floating point.

            1556.7        Macintosh II, A/UX, "cc -O -lm"
			  (Michael McClary).

	    1388.8	  Sun 386i/250, SunOS 4.0.1 C
                          "-O /usr/lib/trig.il".  (James Carrington).

	    1331.93	  Sun 3/60, SunOS 4.0.1, C,
                          "-O4 -f68881 /usr/lib/libm.il"
			  (Bob Elman).

            1204.0        Apollo Domain DN4000, C, "-cpu 3000 -opt 4".
			  (Sam Crupi).

	    1174.66	  Compaq 386/25, SCO Xenix 386 C.
			  (Peter Shieh).

	    1068	  Compaq 386/25, SCO Xenix 386,
			  Metaware High C.  (Robert Wenig).

	    1064.0	  Sun 3/80, SunOS 4.0.3 Beta C
                          "-O3 -f68881 /usr/lib/libm.il".  (James Carrington).

	    1061.4	  Compaq 386/25, SCO Xenix, High C 1.4.
			  (James Carrington).

	    1059.79	  Compaq 386/25, 387/25, High C 1.4,
			  DOS|Extender 2.2, 387 inline code
			  generation.  (Nathan Bender).

	     777.14	  Compaq 386/25, IIT 3C87-25 (387 Compatible),
			  High C 1.5, DOS|Extender 2.2, 387 inline
			  code generation.  (Nathan Bender).

	     751	  Compaq DeskPro 386/33, High C 1.5 + DOS|Extender,
			  387 code generation.	(James Carrington).

	     431.44	  Compaq 386/25, Weitek 3167-25, DOS 3.31,
			  High C 1.4, DOS|Extender, Weitek code generation.
			  (Nathan Bender).

	     344.9	  Compaq 486/25, Metaware High C 1.6, Phar Lap
			  DOS|Extender, in-line floating point.  (Nathan
			  Bender).

	     324.2	  Data General Motorola 88000, 16 Mhz, Gnu C.

             323.1        Sun 4/280, C, "-O4".  (Eric Hill).

	     254	  Compaq SystemPro 486/33, High C 1.5 + DOS|Extender,
			  387 code generation.	(James Carrington).

	     242.8	  Silicon Graphics Personal IRIS, MIPS R2000A,
                          12.5 Mhz, "-O3" (highest level optimisation).
			  (Mike Zentner).

             233.0        Sun SPARCStation 1, C, "-O4", SunOS 4.0.3.
			  (Nathan Bender).

	     187.30	  DEC PMAX 3100, MIPS 2000 chip.
			  (Robert Wenig).

             120.46       Sun SparcStation 2, C, "-O4", SunOS 4.1.1.
			  (John Walker).

             120.21       DEC 3MAX, MIPS 3000, "-O4".

	      98.0	  Intel i860 experimental environment,
			  OS/2, data caching disabled.	(Kern
			  Sibbald).

	      34.9	  Silicon Graphics Indigo², MIPS R4400,
                          175 Mhz, IRIX 5.2, "-O".

	      32.4	  Pentium 133, Windows NT, Microsoft Visual
			  C++ 4.0.

	      17.25	  Silicon Graphics Indigo², MIPS R4400,
                          175 Mhz, IRIX 6.5, "-O3".

	      14.10	  Dell Dimension XPS R100, Pentium II 400 MHz,
			  Windows 98, Microsoft Visual C 5.0.

	      10.7	  Hewlett-Packard Kayak XU 450Mhz Pentium II,
			  Microsoft Visual C++ 6.0, Windows NT 4.0sp3.	(Nathan Bender).

	       5.09	  Sun Ultra 2, UltraSPARC V9, 300 MHz, gcc -O3.
	       
	       0.846	  Dell Inspiron 9100, Pentium 4, 3.4 GHz, gcc -O3.

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

/*  The  program  may  be  run	with  Float defined as either float or
    double.  With IEEE arithmetic, the same answers are generated  for
    either floating point mode.  */

#define Float	 double 	   /* Floating point type used in FFT */

#define Asize	 256		   /* Array edge size */
#define Passes	 20		   /* Number of FFT/Inverse passes */

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<=(b)?(a):(b))

/*

	Multi-dimensional fast Fourier transform

        Adapted from Press et al., "Numerical Recipes in C".

*/

#define SWAP(a,b) tempr=(a); (a)=(b); (b)=tempr

static void fourn(Float data[], int nn[], int ndim, int isign)
{
	register int i1, i2, i3;
	int i2rev, i3rev, ip1, ip2, ip3, ifp1, ifp2;
	int ibit, idim, k1, k2, n, nprev, nrem, ntot;
	Float tempi, tempr;
	double theta, wi, wpi, wpr, wr, wtemp;

	ntot = 1;
	for (idim = 1; idim <= ndim; idim++)
	   ntot *= nn[idim];
	nprev = 1;
	for (idim = ndim; idim >= 1; idim--) {
	   n = nn[idim];
	   nrem = ntot / (n * nprev);
	   ip1 = nprev << 1;
	   ip2 = ip1 * n;
	   ip3 = ip2 * nrem;
	   i2rev = 1;
	   for (i2 = 1; i2 <= ip2; i2 += ip1) {
	      if (i2 < i2rev) {
		 for (i1 = i2; i1 <= i2 + ip1 - 2; i1 += 2) {
		    for (i3 = i1; i3 <= ip3; i3 += ip2) {
		       i3rev = i2rev + i3 - i2;
		       SWAP(data[i3], data[i3rev]);
		       SWAP(data[i3 + 1], data[i3rev + 1]);
		    }
		 }
	      }
	      ibit = ip2 >> 1;
	      while (ibit >= ip1 && i2rev > ibit) {
		 i2rev -= ibit;
		 ibit >>= 1;
	      }
	      i2rev += ibit;
	   }
	   ifp1 = ip1;
	   while (ifp1 < ip2) {
	      ifp2 = ifp1 << 1;
	      theta = isign * 6.28318530717959 / (ifp2 / ip1);
	      wtemp = sin(0.5 * theta);
	      wpr = -2.0 * wtemp * wtemp;
	      wpi = sin(theta);
	      wr = 1.0;
	      wi = 0.0;
	      for (i3 = 1; i3 <= ifp1; i3 += ip1) {
		 for (i1 = i3; i1 <= i3 + ip1 - 2; i1 += 2) {
		    for (i2 = i1; i2 <= ip3; i2 += ifp2) {
		       k1 = i2;
		       k2 = k1 + ifp1;
		       tempr = wr * data[k2] - wi * data[k2 + 1];
		       tempi = wr * data[k2 + 1] + wi * data[k2];
		       data[k2] = data[k1] - tempr;
		       data[k2 + 1] = data[k1 + 1] - tempi;
		       data[k1] += tempr;
		       data[k1 + 1] += tempi;
		    }
		 }
		 wr = (wtemp = wr) * wpr - wi * wpi + wr;
		 wi = wi * wpr + wtemp * wpi + wi;
	      }
	      ifp1 = ifp2;
	   }
	   nprev *= n;
	}
}
#undef SWAP

int main()
{
	int i, j, k, l, m, npasses = Passes, faedge;
	Float *fdata /* , *fd */ ;
	static int nsize[] = {0, 0, 0};
	long fanum, fasize;
	double mapbase, mapscale, /* x, */ rmin, rmax, imin, imax;

	faedge = Asize; 	   /* FFT array edge size */
	fanum = faedge * faedge;   /* Elements in FFT array */
	fasize = ((fanum + 1) * 2 * sizeof(Float)); /* FFT array size */
	nsize[1] = nsize[2] = faedge;

	fdata = (Float *) malloc(fasize);
	if (fdata == NULL) {
           fprintf(stdout, "Can't allocate data array.\n");
	   exit(1);
	}

	/*  Generate data array to process.  */

#define Re(x,y) fdata[1 + (faedge * (x) + (y)) * 2]
#define Im(x,y) fdata[2 + (faedge * (x) + (y)) * 2]

	memset(fdata, 0, fasize);
	for (i = 0; i < faedge; i++) {
	   for (j = 0; j < faedge; j++) {
	      if (((i & 15) == 8) || ((j & 15) == 8))
		 Re(i, j) = 128.0;
	   }
	}

	for (i = 0; i < npasses; i++) {
/*printf("Pass %d\n", i);*/
	   /* Transform image to frequency domain. */
	   fourn(fdata, nsize, 2, 1);

	   /*  Back-transform to image. */
	   fourn(fdata, nsize, 2, -1);
	}

	{
	   double r, ij, ar, ai;
	   rmin = 1e10; rmax = -1e10;
	   imin = 1e10; imax = -1e10;
	   ar = 0;
	   ai = 0;

	   for (i = 1; i <= fanum; i += 2) {
	      r = fdata[i];
	      ij = fdata[i + 1];
	      ar += r;
	      ai += ij;
	      rmin = min(r, rmin);
	      rmax = max(r, rmax);
	      imin = min(ij, imin);
	      imax = max(ij, imax);
	   }
#ifdef DEBUG
           printf("Real min %.4g, max %.4g.  Imaginary min %.4g, max %.4g.\n",
	      rmin, rmax, imin, imax);
           printf("Average real %.4g, imaginary %.4g.\n", 
	      ar / fanum, ai / fanum);
#endif
	   mapbase = rmin;
	   mapscale = 255 / (rmax - rmin);
	}

	/* See if we got the right answers. */

	m = 0;
	for (i = 0; i < faedge; i++) {
	   for (j = 0; j < faedge; j++) {
	      k = (Re(i, j) - mapbase) * mapscale;
	      l = (((i & 15) == 8) || ((j & 15) == 8)) ? 255 : 0;
	      if (k != l) {
		 m++;
		 fprintf(stdout,
                    "Wrong answer at (%d,%d)!  Expected %d, got %d.\n",
		    i, j, l, k);
	      }
	   }
	}
	if (m == 0) {
           fprintf(stdout, "%d passes.  No errors in results.\n", npasses);
	} else {
           fprintf(stdout, "%d passes.  %d errors in results.\n",
	      npasses, m);
	}

#ifdef CAPOUT

	/* Output the result of the transform as a CA Lab pattern
	   file for debugging. */

	{
#define SCRX  322
#define SCRY  200
#define SCRN  (SCRX * SCRY)
	   unsigned char patarr[SCRY][SCRX];
	   FILE *fp;

/*  Map user external state numbers to internal state index  */

#define UtoI(x)     (((((x) >> 1) & 0x7F) | ((x) << 7)) & 0xFF)

	   /* Copy data from FFT buffer to map. */

	   memset(patarr, 0, sizeof patarr);
	   l = (SCRX - faedge) / 2;
	   m = (faedge > SCRY) ? 0 : ((SCRY - faedge) / 2);
	   for (i = 1; i < faedge; i++) {
	      for (j = 0; j < min(SCRY, faedge); j++) {
		 k = (Re(i, j) - mapbase) * mapscale;
		 patarr[j + m][i + l] = UtoI(k);
	      }
	   }

	   /* Dump pattern map to file. */

           fp = fopen("fft.cap", "w");
	   if (fp == NULL) {
              fprintf(stdout, "Cannot open output file.\n");
	      exit(0);
	   }
           putc(':', fp);
	   putc(1, fp);
	   fwrite(patarr, SCRN, 1, fp);
	   putc(6, fp);
	   fclose(fp);
	}
#endif

	return 0;
}
