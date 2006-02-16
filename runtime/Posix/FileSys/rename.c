#include "platform.h"

Int Posix_FileSys_rename (NullString p1, NullString p2) {
        return rename ((char *) p1, (char *) p2);
}
