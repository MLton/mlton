#include "platform.h"

static struct passwd *Posix_SysDB_Passwd_passwd;

C_String_t Posix_SysDB_Passwd_getName(void) {
  return (C_String_t)(Posix_SysDB_Passwd_passwd->pw_name);
}

C_UId_t Posix_SysDB_Passwd_getUId(void) {
  return Posix_SysDB_Passwd_passwd->pw_uid;
}

C_GId_t Posix_SysDB_Passwd_getGId(void) {
  return Posix_SysDB_Passwd_passwd->pw_gid;
}

C_String_t Posix_SysDB_Passwd_getDir(void) {
  return (C_String_t)(Posix_SysDB_Passwd_passwd->pw_dir);
}

C_String_t Posix_SysDB_Passwd_getShell(void) {
  return (C_String_t)(Posix_SysDB_Passwd_passwd->pw_shell);
}

C_Errno_t(C_Int_t) Posix_SysDB_getpwnam(NullString8_t p) {
  return NULL != (Posix_SysDB_Passwd_passwd = getpwnam((const char *) p));
}

C_Errno_t(C_Int_t) Posix_SysDB_getpwuid(C_UId_t u) {
  return NULL != (Posix_SysDB_Passwd_passwd = getpwuid(u));
}
