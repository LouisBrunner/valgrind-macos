#include <stdio.h>
#include <unistd.h>
#include <string>
#include "../memcheck.h"
// Derived from test provided by Timur Iskhodzhanov (bug 280271)


class MyClass
{ 
public:
   ~MyClass() 
   { fprintf(stderr, "destruct MyClass\n"); 
   } 
};

// Two hierarchies using MI, one with no fields,
// the other one with some data.
struct Ae 
{ 
   virtual ~Ae() 
   { fprintf(stderr, "destruct Ae\n"); 
   } 
}; 
struct Be 
{ 
   virtual ~Be() 
   { fprintf(stderr, "destruct Be\n"); 
   } 
}; 
struct Ce : public Ae, public Be 
{ 
   virtual ~Ce() 
   { fprintf(stderr, "destruct Ce\n"); 
   } 
};

struct A 
{ 
   char a;
   A() 
   { a = 'a';
   } 
   virtual ~A() 
   { fprintf(stderr, "destruct A\n"); 
   } 
}; 
struct B 
{ 
   char b;
   B() 
   { b = 'b';
   } 
   virtual ~B() 
   { fprintf(stderr, "destruct B\n"); 
   } 
}; 
struct C : public A, public B 
{ 
   char c;
   C() 
   { c = 'c';
   } 
   virtual ~C() 
   { fprintf(stderr, "destruct C\n"); 
   } 
};

int main() { 
   std::string str = "Valgrind"; // interior ptr.
   std::string str2 = str;
   MyClass *ptr = new MyClass[3]; // interior ptr.
   MyClass *ptr2 = new MyClass[0]; // "interior but exterior ptr".
   // ptr2 points after the chunk, is wrongly considered by memcheck as definitely leaked.

   Be *ptrBCe = new Ce;  // interior ptr.
   Ae *ptrACe = new Ce;  // not an interior pointer.
   B *ptrBC = new C;  // interior ptr.
   A *ptrAC = new C;  // not an interior pointer.


   str2 += " rocks (str2)\n"; // interior ptr.

   VALGRIND_MONITOR_COMMAND("v.set log_output");

   fprintf(stderr, "VALGRIND_DO_LEAK_CHECK\n");
   VALGRIND_DO_LEAK_CHECK; // All possible leaks should be detected, giving only reachable data.

   // Check individually each heuristic
   fprintf(stderr, "leak_check summary heuristics multipleinheritance\n");
   VALGRIND_MONITOR_COMMAND("leak_check summary heuristics multipleinheritance");
   fprintf(stderr, "leak_check summary any heuristics newarray\n");
   VALGRIND_MONITOR_COMMAND("leak_check summary heuristics newarray");
   fprintf(stderr, "leak_check summary heuristics stdstring\n");
   VALGRIND_MONITOR_COMMAND("leak_check summary heuristics stdstring");

   delete [] ptr;
   delete [] ptr2;
   delete ptrBCe;
   delete ptrACe;
   delete ptrBC;
   delete ptrAC;
   fprintf(stderr, "Finished!\n");
   return 0;
}

