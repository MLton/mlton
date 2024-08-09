#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_exece (NullString8_t pNStr,
                                        Array(NullString8_t) aStr,
                                        Array(NullString8_t) eStr) {
  const char      *path;
  char            **args;
  char            **env;
  uintmax_t       aLen;
  uintmax_t       eLen;
  char            *aSaved;
  char            *eSaved;
  int             res;

  path = (const char *) pNStr;
  args = (char **) aStr;
  aLen = GC_getSequenceLength((pointer)aStr);
  aSaved = args[aLen - 1];
  args[aLen - 1] = NULL;
  env = (char **) eStr;
  eLen = GC_getSequenceLength((pointer)eStr);
  eSaved = env[eLen - 1];
  env[eLen - 1] = NULL;
  res = execve (path,
                (char * const *)args,
                (char * const *)env);
  /* exece failed */
  args[aLen - 1] = aSaved;
  env[eLen - 1] = eSaved;
  return (C_Errno_t(C_Int_t))res;
}
