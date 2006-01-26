#include "platform.h"

static inline C_Errno_t(C_SSize_t)
Posix_IO_write (C_Fd_t fd, Pointer b, 
                C_Int_t i, C_Size_t s) {
  return write (fd, (void *) ((char *) b + i), s);
}

C_Errno_t(C_SSize_t)
Posix_IO_writeChar8Arr (C_Fd_t fd, Array(Char8_t) b, 
                        C_Int_t i, C_Size_t s) {
  return Posix_IO_write (fd, (Pointer)b, i, s);
}
C_Errno_t(C_SSize_t)
Posix_IO_writeChar8Vec (C_Fd_t fd, Vector(Char8_t) b, 
                        C_Int_t i, C_Size_t s) {
  return Posix_IO_write (fd, (Pointer)b, i, s);
}
C_Errno_t(C_SSize_t)
Posix_IO_writeWord8Arr (C_Fd_t fd, Array(Word8_t) b, 
                        C_Int_t i, C_Size_t s) {
  return Posix_IO_write (fd, (Pointer)b, i, s);
}
C_Errno_t(C_SSize_t)
Posix_IO_writeWord8Vec (C_Fd_t fd, Vector(Word8_t) b, 
                        C_Int_t i, C_Size_t s) {
  return Posix_IO_write (fd, (Pointer)b, i, s);
}
