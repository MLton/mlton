#include "platform.h"

static struct timeval Socket_timeout;
static struct timeval *Socket_timeoutPtr;

void Socket_setTimeout (C_Time_t sec, C_SUSeconds_t usec) {
  Socket_timeout.tv_sec = sec;
  Socket_timeout.tv_usec = usec;
  Socket_timeoutPtr = &Socket_timeout;
}
C_Time_t Socket_getTimeout_sec (void) {
  return Socket_timeout.tv_sec;
}
C_SUSeconds_t Socket_getTimeout_usec (void) {
  return Socket_timeout.tv_usec;
}
void Socket_setTimeoutNull (void) {
  Socket_timeoutPtr = NULL;
}

C_Errno_t(C_Int_t) Socket_select (Vector(C_Fd_t) read_vec,
                                  Vector(C_Fd_t) write_vec,
                                  Vector(C_Fd_t) except_vec,
                                  Array(C_Int) read_arr,
                                  Array(C_Int) write_arr,
                                  Array(C_Int) except_arr) {
  uintmax_t read_len, write_len, except_len;
  fd_set read_fd_set, write_fd_set, except_fd_set;
  fd_set *read_fds, *write_fds, *except_fds;
  int res;
  
  read_len = GC_getSequenceLength((pointer)read_vec);
  if (read_len > 0) {
    read_fds = &read_fd_set; 
    FD_ZERO(read_fds);
    for (unsigned int i = 0; i < read_len; i++) {
      int fd = ((int *)read_vec)[i];
      FD_SET (fd, read_fds);
    }
  } else {
    read_fds = NULL;
  }
  write_len = GC_getSequenceLength((pointer)write_vec);
  if (write_len > 0) {
    write_fds = &write_fd_set; 
    FD_ZERO(write_fds);
    for (unsigned int i = 0; i < write_len; i++) {
      int fd = ((int *)write_vec)[i];
      FD_SET (fd, write_fds);
    }
  } else {
    write_fds = NULL;
  }
  except_len = GC_getSequenceLength((pointer)except_vec);
  if (except_len > 0) {
    except_fds = &except_fd_set; 
    FD_ZERO(except_fds);
    for (unsigned int i = 0; i < except_len; i++) {
      int fd = ((int *)except_vec)[i];
      FD_SET (fd, except_fds);
    }
  } else {
    except_fds = NULL;
  }
  MLton_initSockets ();
  res = select(FD_SETSIZE, read_fds, write_fds, except_fds, Socket_timeoutPtr);
  if (res == -1) {
    MLton_fixSocketErrno();
    return res;
  }
  if (read_len > 0) {
    for (unsigned int i = 0; i < read_len; i++) {
      int fd = ((int *)read_vec)[i];
      if (FD_ISSET (fd, read_fds)) {
        ((int *)read_arr)[i] = 1;
      }
    }
  }
  if (write_len > 0) {
    for (unsigned int i = 0; i < write_len; i++) {
      int fd = ((int *)write_vec)[i];
      if (FD_ISSET (fd, write_fds)) {
        ((int *)write_arr)[i] = 1;
      }
    }
  }
  if (except_len > 0) {
    for (unsigned int i = 0; i < except_len; i++) {
      int fd = ((int *)except_vec)[i];
      if (FD_ISSET (fd, except_fds)) {
        ((int *)except_arr)[i] = 1;
      }
    }
  }
  return res;
}
