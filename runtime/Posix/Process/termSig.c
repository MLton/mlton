#include "platform.h"

Signal Posix_Process_termSig (Status s) {
        int i;

        i = s;
        return WTERMSIG (i);
}
