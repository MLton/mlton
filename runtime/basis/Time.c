#include "platform.h"

enum {
	DEBUG = 0,
};

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
