#include <sys/utsname.h>
#include "mlton-posix.h"

static struct utsname utsname;

Int Posix_ProcEnv_Uname_uname() {
	return uname(&utsname);
}

Cstring Posix_ProcEnv_Uname_sysname() {
	return (Cstring)utsname.sysname;
}

Cstring Posix_ProcEnv_Uname_nodename() {
	return (Cstring)utsname.nodename;
}

Cstring Posix_ProcEnv_Uname_release() {
	return (Cstring)utsname.release;
}

Cstring Posix_ProcEnv_Uname_version() {
	return (Cstring)utsname.version;
}

Cstring Posix_ProcEnv_Uname_machine() {
	return (Cstring)utsname.machine;
}
