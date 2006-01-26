#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_execp (NullString8_t f, NullString8Array_t a) {
  const char      *file;
  char            *asaved;
  char            **args;
  int             an;
  int             res;
  
  file = (const char *) f;
  args = (char **) a;
  an = GC_getArrayLength ((pointer)a) - 1;
  asaved = args[an];
  args[an] = (char *) NULL;
  res = EXECVP (file, 
                (char * const *)args);
  /* execp failed */
  args[an] = asaved;
  return (C_Errno_t(C_Int_t))res;
}
