#include "platform.h"

static struct tms Posix_ProcEnv_Times_tms;

C_Clock_t Posix_ProcEnv_Times_getUTime(void) {
  return Posix_ProcEnv_Times_tms.tms_utime;
}

C_Clock_t Posix_ProcEnv_Times_getSTime(void) {
  return Posix_ProcEnv_Times_tms.tms_stime;
}

C_Clock_t Posix_ProcEnv_Times_getCUTime(void) {
  return Posix_ProcEnv_Times_tms.tms_cutime;
}

C_Clock_t Posix_ProcEnv_Times_getCSTime(void) {
  return Posix_ProcEnv_Times_tms.tms_cstime;
}

C_Errno_t(C_Clock_t) Posix_ProcEnv_times(void) {
  return times(&Posix_ProcEnv_Times_tms);
}
