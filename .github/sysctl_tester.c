#include <stdio.h>
#include <sys/sysctl.h>

static void test_sysctl(const char *name) {
  int value;
  size_t size = sizeof(value);
  printf("Testing sysctl: %s\n", name);
  if (sysctlbyname(name, &value, &size, NULL, 0) == 0) {
    printf("%s: %d\n", name, value);
  } else {
    perror(name);
  }
}

int main() {
  test_sysctl("hw.cputype");
  test_sysctl("hw.cpusubtype");
  test_sysctl("hw.ncpu");
  test_sysctl("hw.optional.arm.FEAT_SHA3");
  test_sysctl("hw.optional.arm.FEAT_SB");
  test_sysctl("hw.optional.arm.FEAT_LRCPC");
  test_sysctl("hw.optional.arm.FEAT_JSCVT");
  test_sysctl("hw.optional.arm.FEAT_PAuth");
  test_sysctl("hw.optional.arm.FEAT_DIT");
  test_sysctl("hw.optional.arm.FEAT_FP16");
  return 0;
}
