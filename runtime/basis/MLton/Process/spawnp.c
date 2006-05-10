#include "platform.h"

#if HAS_SPAWN

C_Errno_t(C_Int_t) MLton_Process_spawnp (NullString8_t p, NullString8Array_t a) {
  const char      *path;
  const char      *asaved;
  char            **args;
  int             an;
  int             res;

  path = (const char *) p;
  args = (const char **) a;
  an = GC_getArrayLength((pointer)a) - 1;
  asaved = args[an];
  args[an] = (const char *) NULL;
  res = spawnvp (SPAWN_MODE, path, 
                    (const char * const *)args);
  args[an] = asaved;
  return (C_Errno_t(C_Int_t))res;
}

#else

__attribute__ ((noreturn))
C_Errno_t(C_Int_t) MLton_Process_spawnp (__attribute__ ((unused)) NullString8_t p, 
                                         __attribute__ ((unused)) NullString8Array_t a) {
  die ("MLton_Process_spawnp not implemented");
}

#endif
