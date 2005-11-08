#include "platform.h"

#if HAS_SPAWN
Int MLton_Process_spawne (NullString p, Pointer a, Pointer e) {
        char    *path;
        char    *asaved;
        char    *esaved;
        char    **args;
        char    **env;
        int     an;
        int     en;
        int     result;

        path = (char *) p;
        args = (char **) a;
        env = (char **) e;
        an = GC_arrayNumElements(a) - 1;
        asaved = args[an];
        en = GC_arrayNumElements(e) - 1;
        esaved = env[en];
        args[an] = (char *) NULL;
        env[en] = (char *) NULL;
        result = spawnve (SPAWN_MODE, path, 
                                (const char * const *)args,
                                (const char * const *)env);
        args[an] = asaved;
        env[en] = esaved;
        return result;
}
#else
Int MLton_Process_spawne (Pointer p, Pointer a, Pointer e) {
        die ("MLton_Process_spawne not implemented");
}
#endif
