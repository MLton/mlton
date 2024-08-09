#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_execp (NullString8_t fNStr, 
                                        Array(NullString8_t) aStr) {
  const char      *file;
  char            **args;
  uintmax_t       aLen;
  char            *aSaved;
  int             res;

  file = (const char *) fNStr;
  args = (char **) aStr;
  aLen = GC_getSequenceLength((pointer)aStr);
  aSaved = args[aLen - 1];
  args[aLen - 1] = NULL;
  res = execvp (file,
                (char * const *)args);
  /* execp failed */
  args[aLen - 1] = aSaved;
  return (C_Errno_t(C_Int_t))res;
}
