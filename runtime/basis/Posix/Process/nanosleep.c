#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_nanosleep (Ref(C_Time_t) sec, Ref(C_Long_t) nsec) {
  struct timespec rem;
  struct timespec req;
  int res;

  req.tv_sec = *((time_t*)sec);
  req.tv_nsec =*((long*)nsec);
  rem.tv_sec = 0;
  rem.tv_nsec = 0;
  res = nanosleep (&req, &rem);
  *((time_t*)sec) = rem.tv_sec;
  *((long*)nsec) = rem.tv_nsec;
  return res;
}
