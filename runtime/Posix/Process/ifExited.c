#include "platform.h"

Bool Posix_Process_ifExited (Status s) {
        int i;

        i = s;
        return WIFEXITED (i);
}
