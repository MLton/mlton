#include "platform.h"

static struct group *Posix_SysDB_Group_group;

C_String_t Posix_SysDB_Group_getName(void) {
  return (C_String_t)(Posix_SysDB_Group_group->gr_name);
}

C_GId_t Posix_SysDB_Group_getGId(void) {
  return Posix_SysDB_Group_group->gr_gid;
}

C_StringArray_t Posix_SysDB_Group_getMem(void) {
  return (C_StringArray_t)(Posix_SysDB_Group_group->gr_mem);
}

C_Errno_t(C_Int_t) Posix_SysDB_getgrgid(C_GId_t g) {
  return NULL != (Posix_SysDB_Group_group = getgrgid ((gid_t)g));
}

C_Errno_t(C_Int_t) Posix_SysDB_getgrnam(NullString8_t s) {
  return NULL != (Posix_SysDB_Group_group = getgrnam ((const char*)s));
}
