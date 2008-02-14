#include "platform.h"

#if HAS_SPAWN

C_Errno_t(C_PId_t) MLton_Process_spawne (NullString8_t pNStr,
                                         String8_t aStr, 
                                         Array(C_Pointer_t) aPtr,
                                         Vector(C_Size_t) aOff,
                                         String8_t eStr, 
                                         Array(C_Pointer_t) ePtr,
                                         Vector(C_Size_t) eOff) {
  const char      *path;
  char            **args;
  char            **env;
  int             aLen;
  int             eLen;
  C_PId_t         res;

  path = (const char *) pNStr;
  args = (char **) aPtr;
  aLen = GC_getArrayLength((pointer)aPtr);
  for (int i = 0; i < aLen - 1; i++) {
    args[i] = (char *)aStr + ((size_t*)aOff)[i];
  }
  args[aLen - 1] = NULL;
  env = (char **) ePtr;
  eLen = GC_getArrayLength((pointer)ePtr);
  for (int i = 0; i < eLen - 1; i++) {
    env[i] = (char *)eStr + ((size_t*)eOff)[i];
  }
  env[eLen - 1] = NULL;
  res = spawnve (SPAWN_MODE, path, 
                 (const char * const *)args,
                 (const char * const *)env);
  return (C_Errno_t(C_PId_t))res;
}

#else

__attribute__ ((noreturn))
C_Errno_t(C_PId_t) MLton_Process_spawne (__attribute__ ((unused))NullString8_t pNStr,
                                         __attribute__ ((unused))String8_t aStr, 
                                         __attribute__ ((unused))Array(C_Pointer_t) aPtr,
                                         __attribute__ ((unused))Vector(C_Size_t) aOff,
                                         __attribute__ ((unused))String8_t eStr, 
                                         __attribute__ ((unused))Array(C_Pointer_t) ePtr,
                                         __attribute__ ((unused)) Vector(C_Size_t) eOff) {
  die ("MLton_Process_spawne not implemented");
}

#endif
