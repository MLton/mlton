#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_Dirstream_closeDir (C_DirP_t p) {
  return closedir ((DIR *) p);
}

C_Errno_t(C_DirP_t) Posix_FileSys_Dirstream_openDir (NullString8_t p) {
  DIR *res = opendir ((const char *) p);
  return (C_Errno_t(C_DirP_t))res;
}

C_Errno_t(C_String_t) Posix_FileSys_Dirstream_readDir (C_DirP_t d) {
  struct dirent *e;
  char *res;

  e = readdir ((DIR *) d);
  res = (NULL == e) ? NULL : e->d_name;
  return (C_Errno_t(C_String_t))res;
}

void Posix_FileSys_Dirstream_rewindDir (C_DirP_t p) {
  rewinddir ((DIR *) p);
}
