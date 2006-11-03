#include "platform.h"

C_Errno_t(C_Int_t) 
OS_IO_poll (Vector(C_Fd_t) fds, 
            Vector(C_Short_t) eventss, 
            C_NFds_t n, 
            C_Int_t timeout, 
            Array(C_Short_t) reventss) {
  unsigned int i;
  int res;
  struct pollfd ufds[n];

  for (i = 0; i < n; i++) {
    ufds[i].fd = ((const int*)fds)[i];
    ufds[i].events = ((const short*)eventss)[i];
  }
  res = poll (ufds, n, timeout);
  for (i = 0; i < n; i++) {
    ((short*)reventss)[i] = ufds[i].revents;
  }
  return res;
}
