
/* This is really horrible.  It checks that the
   stack unwinder understands DW_CFA_def_cfa_expression.  It is
   the result of compiling this:
	
void bbb ( long x )
{
  __asm__ __volatile__(
      "cmp %0,%0\n\t"
      "jz .Lxyzzy\n"
      ".Lxyzzy:\n\t"
      : : "r"(x) : "cc"
      );
}

void aaa ( long x ) {
  bbb(x);
}

int main ( void )
{
  long *p = malloc(8);
  aaa( *p );
  return 0;
}

and bracketing the cmp/jz insns with a move down/up by 256 of %rsp.
The .jz causes memcheck to complain, hence unwind the stack, but
that cannot be successfully done unless the return address can
be found.  Hence the handwritten CFI below uses
DW_CFA_def_cfa_expression to make that possible.

The CFI below isn't really right in that aaa appears twice
in the backtrace

==12868== Conditional jump or move depends on uninitialised value(s)
==12868==    at 0x400512: bbb (in /home/sewardj/VgTRUNK/trunk/mad0)
==12868==    by 0x400520: aaa (in /home/sewardj/VgTRUNK/trunk/mad0)
==12868==    by 0x400520: aaa (in /home/sewardj/VgTRUNK/trunk/mad0)
==12868==    by 0x400538: main (in /home/sewardj/VgTRUNK/trunk/mad0)

but GDB behaves the same, so I'm not too concerned - indicates
the problem is with the handwritten CFI and not with
V's interpretation of it.
*/	
	
	
	.file	"bad0.c"
	.text


.globl bbb
	.type	bbb, @function
bbb:
.LFB2:
.Lbbb1:
	subq $256,%rsp
.Lbbb2:
	cmp %rdi,%rdi
	jz .Lxyzzy
.Lxyzzy:
	addq $256,%rsp
.Lbbb3:
	ret
.Lbbb4:
.LFE2:
	.size	bbb, .-bbb

	
	
.globl aaa
	.type	aaa, @function
aaa:
.LFB3:
	call	bbb
	rep ; ret
.LFE3:
	.size	aaa, .-aaa
.globl main
	.type	main, @function
main:
.LFB4:
	subq	$8, %rsp
.LCFI0:
	movl	$8, %edi
	call	malloc
	movq	(%rax), %rdi
	call	aaa
	movl	$0, %eax
	addq	$8, %rsp
	ret
.LFE4:
	.size	main, .-main
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	"zR"
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.uleb128 0x1
	.byte	0x3
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE1:

/* start of the FDE for bbb */
.LSFDE1:
	.long	.LEFDE1-.LASFDE1	/* length of FDE */
.LASFDE1:
	.long	.LASFDE1-.Lframe1	/* CIE pointer */
	.long	.LFB2			/* & bbb */
	.long	.LFE2-.LFB2		/* sizeof(bbb) */
	.uleb128 0			/* augmentation length */
	.byte	0x40 + .Lbbb2 - .Lbbb1	/* _advance_loc to .Lbbb2 */

	/* For the section in between .Lbbb2 and .Lbbb3, set the
	CFA to be %rsp+256, and set the return address (dwarf r16)
	to be *(CFA+0). */
	.byte     0x0f	/* _def_cfa_expression */
	.uleb128  .Lexpr1e-.Lexpr1s /* length of expression */
.Lexpr1s:
	.byte	0x77	/* DW_OP_breg7 == %rsp + sleb128(0) */
	.sleb128 0
	.byte	0x40	/* DW_OP_lit16 */
	.byte	0x40	/* DW_OP_lit16 */
	.byte	0x1e	/* DW_OP_mul */
	.byte	0x22	/* DW_OP_plus */
.Lexpr1e:
	.byte 0x90	/* _cfa_offset:	r16 = *(cfa+0) */
	.uleb128 0

	.byte	0x40 + .Lbbb3 - .Lbbb2	/* _advance_loc to .Lbbb3 */

	/* For the section .Lbbb3 to .Lbbb4, should set CFA back to
	something sensible.  This tries to do it but still causes
	GDB to show an extraneous aaa frame on the stack.  Oh well. */
	/* Now set CFA back to %rsp+0 */
	.byte     0x0f	/* _def_cfa_expression */
	.uleb128  .Lexpr2e-.Lexpr2s /* length of expression */
.Lexpr2s:
	.byte	0x77	/* DW_OP_breg7 == %rsp + sleb128(0) */
	.sleb128 0
	.byte	0x30	/* DW_OP_lit0 */
	.byte	0x1c	/* DW_OP_minus */
.Lexpr2e:
	.byte 0x90	/* _cfa_offset:	r16 = *(cfa+0) */
	.uleb128 0

	.byte	0x40 + .Lbbb4 - .Lbbb3	/* _advance_loc to .Lbbb4 */
	.uleb128 0x0			/* ??? */
	.align 8
.LEFDE1:
/* end of the FDE for bbb */
	
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.long	.LFB3
	.long	.LFE3-.LFB3
	.uleb128 0x0
	.align 8
.LEFDE3:
.LSFDE5:
	.long	.LEFDE5-.LASFDE5
.LASFDE5:
	.long	.LASFDE5-.Lframe1
	.long	.LFB4
	.long	.LFE4-.LFB4
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI0-.LFB4
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE5:
	.ident	"GCC: (GNU) 4.1.2 20061115 (prerelease) (SUSE Linux)"
	.section	.note.GNU-stack,"",@progbits
