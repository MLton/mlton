#include "platform.h"

static struct utimbuf utimbuf;

void Posix_FileSys_Utimbuf_setAcTime (C_Time_t t) {
  utimbuf.actime = t;
}

void Posix_FileSys_Utimbuf_setModTime (C_Time_t t) {
  utimbuf.modtime = t;
} 

C_Errno_t(C_Int_t) Posix_FileSys_Utimbuf_utime (NullString8_t s) {
  return utime((const char *)s, &utimbuf);
}
