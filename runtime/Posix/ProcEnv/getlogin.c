#include "platform.h"

Cstring Posix_ProcEnv_getlogin () {
        return (Cstring)(getlogin ());
}
