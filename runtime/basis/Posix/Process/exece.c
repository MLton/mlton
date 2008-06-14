#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_exece (NullString8_t pNStr,
                                        String8_t aStr, 
                                        Array(C_Pointer_t) aPtr,
                                        Vector(C_Size_t) aOff,
                                        String8_t eStr, 
                                        Array(C_Pointer_t) ePtr,
                                        Vector(C_Size_t) eOff) {
  const char      *path;
  char            **args;
  char            **env;
  uintmax_t       aLen;
  uintmax_t       eLen;
  int             res;

  path = (const char *) pNStr;
  args = (char **) aPtr;
  aLen = GC_getArrayLength((pointer)aPtr);
  for (unsigned int i = 0; i < aLen - 1; i++) {
    args[i] = (char *)aStr + ((size_t*)aOff)[i];
  }
  args[aLen - 1] = NULL;
  env = (char **) ePtr;
  eLen = GC_getArrayLength((pointer)ePtr);
  for (unsigned int i = 0; i < eLen - 1; i++) {
    env[i] = (char *)eStr + ((size_t*)eOff)[i];
  }
  env[eLen - 1] = NULL;
  res = EXECVE (path, 
                (char * const *)args, 
                (char * const *)env);
  /* exece failed */
  return (C_Errno_t(C_Int_t))res;
}
