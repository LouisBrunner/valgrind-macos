
/* As discussed on valgrind-users in the thread 
 http://comments.gmane.org/gmane.comp.debugging.valgrind/7535 
 valgrinding Wine running a large win32 app (Picasa) fails with the message 
 
 vex: priv/host-x86/isel.c:510 (doHelperCall): Assertion 
 `typeOfIRExpr(env->type_env, args[i]) == Ity_I32' failed. 

 See http://bugs.kde.org/show_bug.cgi?id=152022

This little fragment used to cause Memcheck to assert, so if the
program runs to completion without dying, the test is passed.
*/


int main ( void ) { 
  __asm__ __volatile__( "subw $0x28, %%sp\n" 
                        "movl $0, 0(%%esp)\n" 
                        "addw $0x28, %%sp" : : : "memory" ); 
  return 0;
} 
 
