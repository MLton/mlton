#define _ISOC99_SOURCE

#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

static struct timeval timeval;

Int Time_gettimeofday () {
	Int res;

	res = gettimeofday (&timeval, (struct timezone*)NULL);
	if (DEBUG)
		fprintf (stderr, "%d = Time_gettimeofday ()\n", (int)res);
	return res;
}

Int Time_sec () {
	return timeval.tv_sec;
}

Int Time_usec () {
	return timeval.tv_usec;
}
