#include "platform.h"

enum {
        DEBUG_DIRSTREAM = FALSE,
};

Int Posix_FileSys_Dirstream_closedir (Cpointer p) {
        Int res;

        res = (Int)(closedir ((DIR *) p));
        if (DEBUG_DIRSTREAM)
                fprintf (stderr, "%d = closedir (0x%08x)\n", (uint)res, (uint)p);
        return res;
}

Cpointer Posix_FileSys_Dirstream_opendir (Pointer p) {
        DIR *res = opendir ((char *) p);
        return (Cpointer)res;
}

Cstring Posix_FileSys_Dirstream_readdir (Cpointer d) {
        struct dirent *e;
        Cstring res;
        
        e = readdir ((DIR *) d);
        res = (Cstring)((NULL == e) ? NULL : e->d_name);
        if (DEBUG_DIRSTREAM)
                fprintf (stderr, "%s = readdir (0x%08x)\n", 
                                ((Cstring)NULL == res) ? "NULL": (char*)res,
                                (uint)d);
        return res;
}

void Posix_FileSys_Dirstream_rewinddir (Cpointer p) {
        if (DEBUG_DIRSTREAM)
                fprintf (stderr, "rewinddir (0x%08x)\n", (uint)p);
        rewinddir ((DIR *) p);
}
