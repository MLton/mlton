#include "platform.h"

Int Posix_ProcEnv_sysconf (Int i) {
        return sysconf (i);
}
