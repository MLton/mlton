#include "platform.h"

static struct group *group;

C_String_t Posix_SysDB_Group_getName(void) {
  return (C_String_t)(group->gr_name);
}

C_GId_t Posix_SysDB_Group_getGId(void) {
  return group->gr_gid;
}

C_StringArray_t Posix_SysDB_Group_getMem(void) {
  return (C_StringArray_t)(group->gr_mem);
}

C_Errno_t(C_Int_t) Posix_SysDB_getgrgid(C_GId_t g) {
  return NULL != (group = getgrgid ((gid_t)g));
}

C_Errno_t(C_Int_t) Posix_SysDB_getgrnam(NullString8_t s) {
  return NULL != (group = getgrnam ((const char*)s));
}
