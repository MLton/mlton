#include "platform.h"

C_Errno_t(C_Int_t) Posix_FileSys_rename (NullString8_t p1, NullString8_t p2) {
  C_Errno_t(C_Int_t) res;
  res = rename ((const char *) p1, (const char *) p2);
#ifdef __MINGW32__
  /* the MinGW rename() function does not remove the destination file 
   * if it exists; we emulate the Unix behavior here. 
   */
  if ((res != 0) && (errno == EEXIST)) {
    res = unlink ((const char *) p2);
    if (res == 0)
      res = rename((const char *) p1, (const char *) p2);
  }
#endif
  return res;
}
