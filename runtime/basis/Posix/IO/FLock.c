#include "platform.h"

static struct flock s_flock;

C_Errno_t(C_Int_t) Posix_IO_FLock_fcntl (C_Fd_t f, C_Int_t cmd) {
  return fcntl (f, cmd, &s_flock);
}

C_Short_t Posix_IO_FLock_getType (void) {
  return s_flock.l_type;
}

C_Short_t Posix_IO_FLock_getWhence (void) {
  return s_flock.l_whence;
}

C_Off_t Posix_IO_FLock_getStart (void) {
  return s_flock.l_start;
}

C_Off_t Posix_IO_FLock_getLen (void) {
        return s_flock.l_len;
}

C_PId_t Posix_IO_FLock_getPId (void) {
        return s_flock.l_pid;
}

void Posix_IO_FLock_setType (C_Short_t x) {
  s_flock.l_type = x;
}

void Posix_IO_FLock_setWhence (C_Short_t x) {
  s_flock.l_whence = x;
} 

void Posix_IO_FLock_setStart (C_Off_t x) {
  s_flock.l_start = x;
} 

void Posix_IO_FLock_setLen (C_Off_t x) {
  s_flock.l_len = x;
} 

void Posix_IO_FLock_setPId (C_PId_t x) {
  s_flock.l_pid = x;
} 
