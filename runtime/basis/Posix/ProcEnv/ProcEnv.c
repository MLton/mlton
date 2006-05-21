#include "platform.h"

C_String_t Posix_ProcEnv_ctermid (void) {
  char *res = ctermid (NULL);
  return (C_String_t)res;
}

C_GId_t Posix_ProcEnv_getegid (void) {
  return getegid ();
}

C_UId_t Posix_ProcEnv_geteuid (void) {
  return geteuid ();
}

C_GId_t Posix_ProcEnv_getgid (void) {
  return getgid ();
}

C_PId_t Posix_ProcEnv_getpid  (void) {
  return getpid ();
}

C_PId_t Posix_ProcEnv_getppid (void) {
  return getppid ();
}

C_PId_t Posix_ProcEnv_getpgrp (void) {
  return getpgrp ();
}

C_UId_t Posix_ProcEnv_getuid (void) {
  return getuid ();
}

C_Errno_t(C_Int_t) Posix_ProcEnv_setgid (C_GId_t g) {
  return setgid (g);
}

C_Errno_t(C_Int_t) Posix_ProcEnv_setpgid (C_PId_t p, C_PId_t g) {
  return setpgid (p, g);
}

C_Errno_t(C_PId_t) Posix_ProcEnv_setsid (void) {
  return setsid ();
}

C_Errno_t(C_Int_t) Posix_ProcEnv_setuid (C_UId_t u) {
  return setuid (u);
}
