#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_execp (NullString8_t fNStr, 
                                        String8_t aStr,
                                        Array(C_Pointer_t) aPtr,
                                        Vector(C_Size_t) aOff) {
  const char      *file;
  char            **args;
  uintmax_t       aLen;
  int             res;

  file = (const char *) fNStr;
  args = (char **) aPtr;
  aLen = GC_getArrayLength((pointer)aPtr);
  for (unsigned int i = 0; i < aLen - 1; i++) {
    args[i] = (char *)aStr + ((size_t*)aOff)[i];
  }
  args[aLen - 1] = NULL;
  res = EXECVP (file, 
                (char * const *)args);
  /* execp failed */
  return (C_Errno_t(C_Int_t))res;
}
