#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

static struct utsname utsname;

Int Posix_ProcEnv_Uname_uname () {
	Int res;
	
	res = uname (&utsname);
	if (DEBUG)
		fprintf (stderr, "%d = Posix_ProcEnv_Uname_uname ()\n",
				(int)res);
	return res;
}

Cstring Posix_ProcEnv_Uname_sysname () {
	return (Cstring)utsname.sysname;
}

Cstring Posix_ProcEnv_Uname_nodename () {
	return (Cstring)utsname.nodename;
}

Cstring Posix_ProcEnv_Uname_release () {
	return (Cstring)utsname.release;
}

Cstring Posix_ProcEnv_Uname_version () {
	return (Cstring)utsname.version;
}

Cstring Posix_ProcEnv_Uname_machine () {
	return (Cstring)utsname.machine;
}
