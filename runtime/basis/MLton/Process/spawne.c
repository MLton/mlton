#include "platform.h"

#if HAS_SPAWN
C_Errno_t(C_Int_t) MLton_Process_spawne (NullString8_t p, NullString8Array_t a, NullString8Array_t e) {
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
  an = GC_arrayNumElements((pointer)a) - 1;
  asaved = args[an];
  en = GC_arrayNumElements((pointer)e) - 1;
  esaved = env[en];
  args[an] = (char *) NULL;
  env[en] = (char *) NULL;
  res = spawnve (SPAWN_MODE, path, 
                 (const char * const *)args,
                 (const char * const *)env);
  args[an] = asaved;
  env[en] = esaved;
  return res;
}
#else
C_Errno_t(C_Int_t) MLton_Process_spawne (__attribute__ ((unused)) NullString8_t p, 
                                         __attribute__ ((unused)) NullString8Array_t a, 
                                         __attribute__ ((unused)) NullString8Array_t e) {
  die ("MLton_Process_spawne not implemented");
}
#endif
