#include "platform.h"

Int Posix_Process_alarm (Int i) {
        return alarm (i);
}
