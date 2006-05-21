#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_exece (NullString8_t p, NullString8Array_t a, NullString8Array_t e) {
  const char      *path;
  char            *asaved;
  char            *esaved;
  char            **args;
  char            **env;
  int             an;
  int             en;
  int             res;

  path = (const char *) p;
  args = (char **) a;
  env = (char **) e;
  an = GC_getArrayLength ((pointer)a) - 1;
  asaved = args[an];
  en = GC_getArrayLength ((pointer)e) - 1;
  esaved = env[en];
  args[an] = (char *) NULL;
  env[en] = (char *) NULL;
  res = EXECVE (path, 
                (char * const *)args, 
                (char * const *)env);
  /* exece failed */
  args[an] = asaved;
  env[en] = esaved;
  return (C_Errno_t(C_Int_t))res;
}
