#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

Cstring Posix_ProcEnv_ctermid () {
	return (Cstring)(ctermid (NULL));
}

Gid Posix_ProcEnv_getegid () {
	return getegid ();
}

Uid Posix_ProcEnv_geteuid () {
	return geteuid ();
}

Gid Posix_ProcEnv_getgid () {
	return getgid ();
}

Pid Posix_ProcEnv_getpid  () {
	return getpid ();
}

Pid Posix_ProcEnv_getppid () {
	return getppid ();
}

Uid Posix_ProcEnv_getuid () {
	return getuid ();
}

Int Posix_ProcEnv_setgid (Gid g) {
	return setgid (g);
}

Int Posix_ProcEnv_setpgid (Pid p, Gid g) {
	return setpgid (p, g);
}

Pid Posix_ProcEnv_setsid () {
	return setsid ();
}

Int Posix_ProcEnv_setuid (Uid u) {
	return setuid (u);
}
