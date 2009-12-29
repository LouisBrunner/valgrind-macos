// Test program to verify whether the Valgrind header files compile fine
// with a C++ compiler.


#include <stdio.h>
#include <stdlib.h>
#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_vki.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_errormgr.h"
#include "pub_tool_options.h"
#include "pub_tool_machine.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_seqmatch.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_options.h"

#if defined(VGO_darwin)
int CheckSys()
{
  return SysRes_MACH;
}
#endif

void CheckAssert(int x)
{
  tl_assert(x);
  tl_assert2(x, "fail");
}

int main(int argc, char** argv)
{
  fprintf(stderr, "Compilation succeeded.\n");
  return 0;
}

void VG_(assert_fail)(Bool isCore, const Char* expr, const Char* file,
                      Int line, const Char* fn, const HChar* format, ... )
{
  abort();
}
