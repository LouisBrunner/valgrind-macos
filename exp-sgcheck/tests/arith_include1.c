#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>
#include <string.h>

static jmp_buf TTT_jmpbuf;

void SEGV_handler(int signum)
{
   //fprintf(stderr, "segv caught\n");
   __builtin_longjmp(TTT_jmpbuf, 1);
}

int up[10], up2[10];

