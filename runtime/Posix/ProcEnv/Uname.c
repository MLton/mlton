#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

static struct utsname mlton_utsname;

Int Posix_ProcEnv_Uname_uname () {
        Int res;
        
        res = uname (&mlton_utsname);
        if (DEBUG)
                fprintf (stderr, "%d = Posix_ProcEnv_Uname_uname ()\n",
                                (int)res);
        return res;
}

Cstring Posix_ProcEnv_Uname_sysname () {
        return (Cstring)mlton_utsname.sysname;
}

Cstring Posix_ProcEnv_Uname_nodename () {
        return (Cstring)mlton_utsname.nodename;
}

Cstring Posix_ProcEnv_Uname_release () {
        return (Cstring)mlton_utsname.release;
}

Cstring Posix_ProcEnv_Uname_version () {
        return (Cstring)mlton_utsname.version;
}

Cstring Posix_ProcEnv_Uname_machine () {
        return (Cstring)mlton_utsname.machine;
}
