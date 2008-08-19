#include "platform.h"

#if HAS_SPAWN

C_Errno_t(C_PId_t) MLton_Process_spawne (NullString8_t pNStr,
                                         Array(NullString8_t) aStr,
                                         Array(NullString8_t) eStr) {
  const char      *path;
  char            **args;
  char            **env;
  int             aLen;
  int             eLen;
  char            *aSaved;
  char            *eSaved;
  C_PId_t         res;

  path = (const char *) pNStr;
  args = (char **) aStr;
  aLen = GC_getArrayLength((pointer)aStr);
  aSaved = args[aLen - 1];
  args[aLen - 1] = NULL;
  env = (char **) eStr;
  eLen = GC_getArrayLength((pointer)eStr);
  eSaved = env[eLen - 1];
  env[eLen - 1] = NULL;
  res = spawnve (SPAWN_MODE, path, 
                 (const char * const *)args,
                 (const char * const *)env);
  /* spawnve failed */
  args[aLen - 1] = aSaved;
  env[eLen - 1] = eSaved;
  return (C_Errno_t(C_PId_t))res;
}

#else

__attribute__ ((noreturn))
C_Errno_t(C_PId_t) MLton_Process_spawne (__attribute__ ((unused))NullString8_t pNStr,
                                         __attribute__ ((unused))Array(NullString8_t) aStr,
                                         __attribute__ ((unused))Array(NullString8_t) ePtr) {
  die ("MLton_Process_spawne not implemented");
}

#endif
