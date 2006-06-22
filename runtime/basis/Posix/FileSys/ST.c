#include "platform.h"

Bool Posix_FileSys_ST_isBlk (C_Mode_t m) {
  return 0 != S_ISBLK(m);
}

Bool Posix_FileSys_ST_isChr (C_Mode_t m) {
  return 0 != S_ISCHR(m);
}

Bool Posix_FileSys_ST_isDir (C_Mode_t m) {
  return 0 != S_ISDIR(m);
}

Bool Posix_FileSys_ST_isFIFO (C_Mode_t m) {
  return 0 != S_ISFIFO(m);
}

Bool Posix_FileSys_ST_isLink (C_Mode_t m) {
  return 0 != S_ISLNK(m);
}

Bool Posix_FileSys_ST_isReg (C_Mode_t m) {
  return 0 != S_ISREG(m);
}

Bool Posix_FileSys_ST_isSock (C_Mode_t m) {
  return 0 != S_ISSOCK(m);
}
