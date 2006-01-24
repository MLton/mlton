#include "platform.h"

#if HAS_SPAWN
Int MLton_Process_spawnp (NullString p, Pointer a) {
        char    *path;
        char    *asaved;
        char    **args;
        int     an;
        int     result;

        path = (char *) p;
        args = (char **) a;
        an = GC_arrayNumElements(a) - 1;
        asaved = args[an];
        args[an] = (char *) NULL;
        result = spawnvp (SPAWN_MODE, path, (const char * const *)args);
        args[an] = asaved;
        return result;
}
#else
Int MLton_Process_spawnp (__attribute__ ((unused)) Pointer p, 
                          __attribute__ ((unused)) Pointer a) {
        die ("MLton_Process_spawnp not implemented");
}
#endif
