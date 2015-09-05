/* It's possible that people #include valgrind.h in files compiled with
 * -ansi.  So valgrind.h shouldn't contain any code that won't pass -ansi,
 * such as C++ style "//" comments.  This test ensures that.  So the test is
 * really that it compiles ok, rather than it runs ok.  From bug report
 * #103182. */

#include "valgrind.h"
#include "../../memcheck/memcheck.h"
#include "../../helgrind/helgrind.h"
#include "../../callgrind/callgrind.h"
#include "../../drd/drd.h"

int main(void)
{
   return 0;
}
