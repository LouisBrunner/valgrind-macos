#include <stdio.h>
#include <unistd.h>
#include <string>
#include <sstream>
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

std::string str;
std::string str2;
MyClass *ptr;
MyClass *ptr2;
Be *ptrBCe;
Ae *ptrACe;
B *ptrBC;
A *ptrAC;

char who_points_at_cmd[100];

void doit(void)
{
  str = "Valgrind"; // interior ptr.
  str2 = str;
  ptr = new MyClass[3]; // interior ptr.
  
  // prepare the who_points_at cmd we will run.
  // Do it here to avoid having ptr or its exterior ptr kept in a register.
  sprintf(who_points_at_cmd, "who_points_at %p 20", (char*)ptr - sizeof(void*));

  ptr2 = new MyClass[0]; // "interior but exterior ptr".
  // ptr2 points after the chunk, is wrongly considered by memcheck as definitely leaked.

  ptrBCe = new Ce;  // interior ptr.
  ptrACe = new Ce;  // not an interior pointer.
  ptrBC = new C;  // interior ptr.
  ptrAC = new C;  // not an interior pointer.
  
  
  str2 += " rocks (str2)\n"; // interior ptr.
}


int main() { 

   doit();
   (void) VALGRIND_MONITOR_COMMAND("v.set log_output");

   fprintf(stderr, "VALGRIND_DO_LEAK_CHECK\n");
   VALGRIND_DO_LEAK_CHECK; // All possible leaks should be detected, giving only reachable data.

   // Check individually each heuristic
   fprintf(stderr, "leak_check summary heuristics multipleinheritance\n");
   (void) VALGRIND_MONITOR_COMMAND("leak_check summary heuristics multipleinheritance");
   fprintf(stderr, "leak_check summary any heuristics newarray\n");
   (void) VALGRIND_MONITOR_COMMAND("leak_check summary heuristics newarray");
   fprintf(stderr, "leak_check summary heuristics stdstring\n");
   (void) VALGRIND_MONITOR_COMMAND("leak_check summary heuristics stdstring");

   // Test the who_points_at when the block is pointed to with an interior ptr.
   (void) VALGRIND_MONITOR_COMMAND(who_points_at_cmd);

   delete [] ptr;
   delete [] ptr2;
   delete ptrBCe;
   delete ptrACe;
   delete ptrBC;
   delete ptrAC;
   fprintf(stderr, "Finished!\n");
   return 0;
}

