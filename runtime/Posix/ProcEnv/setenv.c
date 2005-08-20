#include "platform.h"

Int Posix_ProcEnv_setenv (NullString s, NullString v) {
        return setenv ((char *)s, (char *)v, 1);
}
