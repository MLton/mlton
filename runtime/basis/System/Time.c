#include "platform.h"

/* XXX global state */
static struct timeval timeval;

C_Int_t Time_getTimeOfDay (void) {
  int res;
  res = gettimeofday (&timeval, (struct timezone*)NULL);
  return res;
}

C_Time_t Time_sec (void) { return timeval.tv_sec; }

C_SUSeconds_t Time_usec (void) { return timeval.tv_usec; }
