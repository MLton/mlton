#include "platform.h"

static struct utsname mlton_utsname;

C_String_t Posix_ProcEnv_Uname_getSysName () {
  return (C_String_t)mlton_utsname.sysname;
}

C_String_t Posix_ProcEnv_Uname_getNodeName () {
  return (C_String_t)mlton_utsname.nodename;
}

C_String_t Posix_ProcEnv_Uname_getRelease () {
  return (C_String_t)mlton_utsname.release;
}

C_String_t Posix_ProcEnv_Uname_getVersion () {
  return (C_String_t)mlton_utsname.version;
}

C_String_t Posix_ProcEnv_Uname_getMachine () {
  return (C_String_t)mlton_utsname.machine;
}

C_Errno_t(C_Int_t) Posix_ProcEnv_uname () {
  return uname (&mlton_utsname);
}
