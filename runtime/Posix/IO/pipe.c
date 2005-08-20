#include "platform.h"

Int Posix_IO_pipe (Pointer fds) {
        return pipe ((int *) fds);
}
