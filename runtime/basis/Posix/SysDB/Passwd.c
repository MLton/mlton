#include "platform.h"

static struct passwd *passwd;

C_String_t Posix_SysDB_Passwd_getName(void) {
  return (C_String_t)(passwd->pw_name);
}

C_UId_t Posix_SysDB_Passwd_getUId(void) {
  return passwd->pw_uid;
}

C_GId_t Posix_SysDB_Passwd_getGId(void) {
  return passwd->pw_gid;
}

C_String_t Posix_SysDB_Passwd_getDir(void) {
  return (C_String_t)(passwd->pw_dir);
}

C_String_t Posix_SysDB_Passwd_getShell(void) {
  return (C_String_t)(passwd->pw_shell);
}

C_Errno_t(C_Int_t) Posix_SysDB_getpwnam(NullString8_t p) {
  return NULL != (passwd = getpwnam((const char *) p));
}

C_Errno_t(C_Int_t) Posix_SysDB_getpwuid(C_UId_t u) {
  return NULL != (passwd = getpwuid(u));
}
