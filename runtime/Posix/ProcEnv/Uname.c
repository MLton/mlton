#include "platform.h"

static struct utsname mlton_utsname;

C_String_t Posix_ProcEnv_Uname_getSysName (void) {
  return (C_String_t)mlton_utsname.sysname;
}

C_String_t Posix_ProcEnv_Uname_getNodeName (void) {
  return (C_String_t)mlton_utsname.nodename;
}

C_String_t Posix_ProcEnv_Uname_getRelease (void) {
  return (C_String_t)mlton_utsname.release;
}

C_String_t Posix_ProcEnv_Uname_getVersion (void) {
  return (C_String_t)mlton_utsname.version;
}

C_String_t Posix_ProcEnv_Uname_getMachine (void) {
  return (C_String_t)mlton_utsname.machine;
}

C_Errno_t(C_Int_t) Posix_ProcEnv_uname (void) {
  return uname (&mlton_utsname);
}
