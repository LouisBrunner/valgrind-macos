// This is not a thorough test, but at least it's something. It's hard to do
// much better because the exact number of instructions executed is
// unpredictable.

#include "../cachegrind.h"

int main(void) {
   CACHEGRIND_START_INSTRUMENTATION;   // warning with `--instr-at-start=yes`
   CACHEGRIND_STOP_INSTRUMENTATION;
   CACHEGRIND_START_INSTRUMENTATION;
   CACHEGRIND_START_INSTRUMENTATION;   // warning
   CACHEGRIND_STOP_INSTRUMENTATION;
   CACHEGRIND_STOP_INSTRUMENTATION;    // warning
}
