#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

static struct timeval timeval;

C_Int_t Time_getTimeOfDay (void) {
  int res;
  res = gettimeofday (&timeval, (struct timezone*)NULL);
  if (DEBUG)
    fprintf (stderr, "%d = Time_gettimeofday ()\n", res);
  return res;
}

C_Time_t Time_sec (void) { return timeval.tv_sec; }

C_SUSeconds_t Time_usec (void) { return timeval.tv_usec; }
