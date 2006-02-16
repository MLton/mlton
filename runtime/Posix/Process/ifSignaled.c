#include "platform.h"

Bool Posix_Process_ifSignaled (Status s) {
        int i;

        i = s;
        return WIFSIGNALED (i);
}
