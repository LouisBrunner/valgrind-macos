
/* Point of this test is to generate some very long 'stabs' debug
   strings, via the use of these exponentially large template types.
   When compiled with -gstabs, this causes 3.1.1 to segfault at
   startup.
*/

#include <stdio.h>

template <class T, class U>
class Stack
{
   public:
      Stack(int = 10) { size=5; top=6; stackPtr=(T*)6;  }; 
      ~Stack() { }
      int push(const T&, const U&); 
      int popT(T&);
      int popU(U&);
      int isEmpty() const { return top == -1; } 
      int isFull() const { return top == size - 1; } 
   private:
      int size;
      int top;  
      T* stackPtr;  
} ;

typedef  Stack<int,char>  Foo;
typedef  Stack<Foo,Foo>  Foo2;
typedef  Stack<Foo2,Foo2> Foo3;
typedef  Stack<Foo3,Foo3> Foo4;
typedef  Stack<Foo4,Foo4> Foo5;
typedef  Stack<Foo5,Foo5> Foo6;
typedef  Stack<Foo6,Foo6> Foo7;
typedef  Stack<Foo7,Foo7> Foo8;
typedef  Stack<Foo8,Foo8> Foo9;
typedef  Stack<Foo9,Foo9> Foo10;
typedef  Stack<Foo10,Foo10> Foo11;
typedef  Stack<Foo11,Foo11> Foo12;

int main ( int argc, char** argv )
{
  Stack<Foo12,Foo12> * x = new Stack<Foo12,Foo12>(3);
  if (x == NULL)
    printf("It's NULL (?!)\n");
  else
    printf("It's not NULL.  How DULL.\n");
  delete x;
  return 0;
}
