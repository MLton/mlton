#include <sys/time.h>
#include <unistd.h>
#include "mlton-basis.h"

static struct timeval timeval;

Int Time_gettimeofday () {
	return gettimeofday (&timeval, (struct timezone*)NULL);
}

Int Time_sec () {
	return timeval.tv_sec;
}

Int Time_usec () {
	return timeval.tv_usec;
}
