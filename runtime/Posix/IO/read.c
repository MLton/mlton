#include "platform.h"

static inline C_Errno_t(C_SSize_t)
Posix_IO_read (C_Fd_t fd, Pointer b,
               C_Int_t i, C_Size_t s) {
  return read (fd, (void *) ((char *) b + i), s);
}

C_Errno_t(C_SSize_t)
Posix_IO_readChar8 (C_Fd_t fd, Array(Char8) b,
                   C_Int_t i, C_Size_t s) {
  return Posix_IO_read (fd, (Pointer)b, i, s);
}
C_Errno_t(C_SSize_t)
Posix_IO_readWord8 (C_Fd_t fd, Array(Word8) b,
                    C_Int_t i, C_Size_t s) {
  return Posix_IO_read (fd, (Pointer)b, i, s);
}
