#include <sys/times.h>
#include "mlton-posix.h"

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
