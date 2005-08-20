#include "platform.h"

Int Posix_IO_fcntl3 (Fd f, Int i, Int j) {
        return fcntl (f, i, j);
}
