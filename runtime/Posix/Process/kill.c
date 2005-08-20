#include "platform.h"

Int Posix_Process_kill (Pid p, Signal s) {
        return kill (p, s);
}
