#include "platform.h"

Int Posix_FileSys_fpathconf (Fd f, Int n) {
        return fpathconf (f, n);
}
