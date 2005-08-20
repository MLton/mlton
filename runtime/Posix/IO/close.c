#include "platform.h"

Int Posix_IO_close (Fd f) {
        return close (f);
}
