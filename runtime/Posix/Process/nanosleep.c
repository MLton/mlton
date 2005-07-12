#include "platform.h"

Int Posix_Process_nanosleep (Pointer sec, Pointer nsec) {
	struct timespec rem;
	struct timespec req;
	int res;

	req.tv_sec = *(Int*)sec;
	req.tv_nsec =*(Int*)nsec;
	rem.tv_sec = 0;
	rem.tv_nsec = 0;
	res = nanosleep (&req, &rem);
	if (FALSE)
		fprintf (stderr, "res = %d  sec = %d  nsec = %d\n",
				res, (int)rem.tv_sec, (int)rem.tv_nsec);
	*(Int*)sec = rem.tv_sec;
	*(Int*)nsec = rem.tv_nsec;
	return res;
}
