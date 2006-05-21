#include "platform.h"

Bool_t Posix_FileSys_ST_isBlk (C_Mode_t m) {
  return S_ISBLK(m);
}

Bool_t Posix_FileSys_ST_isChr (C_Mode_t m) {
  return S_ISCHR(m);
}

Bool_t Posix_FileSys_ST_isDir (C_Mode_t m) {
  return S_ISDIR(m);
}

Bool_t Posix_FileSys_ST_isFIFO (C_Mode_t m) {
  return S_ISFIFO(m);
}

Bool_t Posix_FileSys_ST_isLink (C_Mode_t m) {
  return S_ISLNK(m);
}

Bool_t Posix_FileSys_ST_isReg (C_Mode_t m) {
  return S_ISREG(m);
}

Bool_t Posix_FileSys_ST_isSock (C_Mode_t m) {
  return S_ISSOCK(m);
}
