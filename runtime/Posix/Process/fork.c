#include "platform.h"

Pid Posix_Process_fork () {
        return fork ();
}
