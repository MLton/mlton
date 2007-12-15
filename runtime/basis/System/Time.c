#include "platform.h"

C_Int_t Time_getTimeOfDay (Ref(C_Time_t) sec, Ref(C_SUSeconds_t) usec) {
  struct timeval timeval;
  int res;
  res = gettimeofday (&timeval, (struct timezone*)NULL);
  if (! res) {
    *((C_Time_t*)sec) = timeval.tv_sec;
    *((C_SUSeconds_t*)usec) = timeval.tv_usec;
  }
  return res;
}
