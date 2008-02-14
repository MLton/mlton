#include "platform.h"

#if HAS_SPAWN

C_Errno_t(C_PId_t) MLton_Process_spawnp (NullString8_t pNStr, 
                                         String8_t aStr,
                                         Array(C_Pointer_t) aPtr,
                                         Vector(C_Size_t) aOff) {
  const char      *path;
  char            **args;
  int             aLen;
  C_PId_t         res;

  path = (const char *) pNStr;
  args = (char **) aPtr;
  aLen = GC_getArrayLength((pointer)aPtr);
  for (int i = 0; i < aLen - 1; i++) {
    args[i] = (char *)aStr + ((size_t*)aOff)[i];
  }
  args[aLen - 1] = NULL;
  res = spawnvp (SPAWN_MODE, path, 
                 (const char * const *)args);
  return (C_Errno_t(C_PId_t))res;
}

#else

__attribute__ ((noreturn))
C_Errno_t(C_PId_t) MLton_Process_spawnp (__attribute__ ((unused)) NullString8_t pNStr, 
                                         __attribute__ ((unused)) String8_t aStr,
                                         __attribute__ ((unused)) Array(C_Pointer_t) aPtr,
                                         __attribute__ ((unused)) Vector(C_Size_t) aOff) {
  die ("MLton_Process_spawnp not implemented");
}

#endif
