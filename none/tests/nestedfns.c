
/* This is a test program from Lee Kindness which used to fail on V
   because gcc implements the nested function mumbo jumbo using self
   modifying code on the stack, at least on x86 and amd64.  It now
   works transparently because by default V now generates
   self-checking translations for translations taken from stack-like
   segments.
*/

#include <stdio.h> 
 
 static void call_func(void (*sel)(void)) 
 { 
    sel(); 
 } 
 
 void test1() 
 { 
    void test1_inner() 
    { 
       printf( "Inside test1\n" ); 
    } 
    call_func( test1_inner ); 
 } 
 
 void test2() 
 { 
    void test2_inner() 
    { 
       printf( "Inside test2\n" ); 
    } 
    call_func( test2_inner ); 
 } 
 
 int main(int argc, char** argv) 
 { 
    test1(); 
    test2(); 
    return( 0 ); 
 } 
 
