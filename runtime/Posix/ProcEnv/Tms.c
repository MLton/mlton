#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

static struct tms tms;

Int Posix_ProcEnv_Tms_utime() {
	return tms.tms_utime;
}

Int Posix_ProcEnv_Tms_stime() {
	return tms.tms_stime;
}

Int Posix_ProcEnv_Tms_cutime() {
	return tms.tms_cutime;
}

Int Posix_ProcEnv_Tms_cstime() {
	return tms.tms_cstime;
}

Int Posix_ProcEnv_times() {
	return times(&tms);
}
