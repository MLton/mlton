#include "platform.h"

static struct tms tms;

C_Clock_t Posix_ProcEnv_Times_getUTime(void) {
  return tms.tms_utime;
}

C_Clock_t Posix_ProcEnv_Times_getSTime(void) {
  return tms.tms_stime;
}

C_Clock_t Posix_ProcEnv_Times_getCUTime(void) {
  return tms.tms_cutime;
}

C_Clock_t Posix_ProcEnv_Times_getCSTime(void) {
  return tms.tms_cstime;
}

C_Errno_t(C_Clock_t) Posix_ProcEnv_times(void) {
  return times(&tms);
}
