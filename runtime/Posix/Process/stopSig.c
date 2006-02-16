#include "platform.h"

Signal Posix_Process_stopSig (Status s) {
        int i;

        i = s;
        return WSTOPSIG (i);
}
