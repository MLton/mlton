#include "platform.h"

static struct passwd *passwd;

Cstring Posix_SysDB_Passwd_name() {
        return (Cstring)passwd->pw_name;
}

Uid Posix_SysDB_Passwd_uid() {
        return passwd->pw_uid;
}

Gid Posix_SysDB_Passwd_gid() {
        return passwd->pw_gid;
}

Cstring Posix_SysDB_Passwd_dir() {
        return (Cstring)passwd->pw_dir;
}

Cstring Posix_SysDB_Passwd_shell() {
        return (Cstring)passwd->pw_shell;
}

Bool Posix_SysDB_getpwnam(Pointer p) {
        return NULL != (passwd = getpwnam((char *) p));
}

Bool Posix_SysDB_getpwuid(Uid u) {
        return NULL != (passwd = getpwuid(u));
}
