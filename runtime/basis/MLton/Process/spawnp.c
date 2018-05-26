#include "platform.h"

#if HAS_SPAWN

C_Errno_t(C_PId_t) MLton_Process_spawnp (NullString8_t pNStr, 
                                         Array(NullString8_t) aStr) {
  const char      *path;
  char            **args;
  int             aLen;
  char            *aSaved;
  C_PId_t         res;

  path = (const char *) pNStr;
  args = (char **) aStr;
  aLen = GC_getSequenceLength((pointer)aStr);
  aSaved = args[aLen - 1];
  args[aLen - 1] = NULL;
  res = spawnvp (SPAWN_MODE, path, 
                 (const char * const *)args);
  /* spawnvp failed */
  args[aLen - 1] = aSaved;
  return (C_Errno_t(C_PId_t))res;
}

#else

__attribute__ ((noreturn))
C_Errno_t(C_PId_t) MLton_Process_spawnp (__attribute__ ((unused)) NullString8_t pNStr, 
                                         __attribute__ ((unused)) Array(NullString8_t) aStr) {
  die ("MLton_Process_spawnp not implemented");
}

#endif
