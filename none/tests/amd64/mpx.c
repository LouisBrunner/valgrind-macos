int
main (int argc, char **argv)
{
  // Since MPX is disabled all these are just NOPS.
  // Some of these instructions are just random.
  // Once the GCC support is merged creating real test cases will be easier.
  // http://gcc.gnu.org/wiki/Intel%20MPX%20support%20in%20the%20GCC%20compiler

  // This is what ld.so does in _dl_runtime_resolve to save the bnds.
  asm ("bndmov %bnd0, (%rsp)");
  asm ("bndmov %bnd1, 16(%rsp)");
  asm ("bndmov %bnd2, 32(%rsp)");
  asm ("bndmov %bnd3, 48(%rsp)");

  // Create a bnd, check lower and upper...
  asm ("bndmk (%rax,%rdx), %bnd0");
  asm ("bndcl (%rax,%rdi,4), %bnd0");
  asm ("bndcu 3(%rax,%rdi,4), %bnd0");
  asm ("bndcn 3(%rax,%rdi,4), %bnd0");

  // Load bnd pointer and update...
  asm ("bndldx 3(%rbx,%rdx), %bnd2");
  asm ("bndstx %bnd2, 3(,%r12,1)");

  // "bnd" prefixed call, return and jmp...
  asm ("bnd call foo\n\
        bnd jmp  end\n\
        foo: bnd ret\n\
        end: nop");

  // And set the bnds back...
  asm ("bndmov 48(%rsp), %bnd3");
  asm ("bndmov 32(%rsp), %bnd2");
  asm ("bndmov 16(%rsp), %bnd1");
  asm ("bndmov (%rsp), %bnd0");

  return 0;
}
