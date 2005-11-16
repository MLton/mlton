#include "platform.h"

Cstring Posix_ProcEnv_getlogin () {
        char *res = getlogin ();
        return (Cstring)(res);
}
