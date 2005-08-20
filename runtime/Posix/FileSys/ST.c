#include "platform.h"

Bool Posix_FileSys_ST_isBlk (Word w) {
        return S_ISBLK(w);
}

Bool Posix_FileSys_ST_isChr (Word w) {
        return S_ISCHR (w);
}

Bool Posix_FileSys_ST_isDir (Word w) {
        return S_ISDIR (w);
}

Bool Posix_FileSys_ST_isFIFO (Word w) {
        return S_ISFIFO (w);
}

Bool Posix_FileSys_ST_isLink (Word w) {
        return S_ISLNK (w);
}

Bool Posix_FileSys_ST_isReg (Word w) {
        return S_ISREG (w);
}

Bool Posix_FileSys_ST_isSock (Word w) {
        return S_ISSOCK (w);
}
