#include <grp.h>
#include <sys/types.h>
#include "mlton-posix.h"
#include "my-lib.h"

static struct group *group;

Cstring Posix_SysDB_Group_name() {
	return (Cstring)group->gr_name;
}

Gid Posix_SysDB_Group_gid() {
	return group->gr_gid;
}

CstringArray Posix_SysDB_Group_mem() {
	return (CstringArray)group->gr_mem;
}

Bool Posix_SysDB_getgrgid(Gid g) {
 	return NULL != (group = getgrgid((gid_t)g));
}

Bool Posix_SysDB_getgrnam(NullString s) {
 	return NULL != (group = getgrnam((char*)s));
}
