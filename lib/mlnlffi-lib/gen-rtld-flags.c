#include <stdio.h>
#include <dlfcn.h>

#ifndef RTLD_LOCAL
#define RTLD_LOCAL 0
#endif

int main(int argc, char *argv[])
{
  printf("structure RTLDFlags = struct\n");
  printf("   val RTLD_GLOBAL = 0wx%x\n", RTLD_GLOBAL);
  printf("   val RTLD_LAZY = 0wx%x\n", RTLD_LAZY);
  printf("   val RTLD_LOCAL = 0wx%x\n", RTLD_LOCAL);
  printf("   val RTLD_NOW = 0wx%x\n", RTLD_NOW);
  printf("end\n");

  return 0;
}
