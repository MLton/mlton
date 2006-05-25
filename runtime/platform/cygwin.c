#define _GNU_SOURCE

#include "platform.h"

#include "mkdir2.c"
#include "mmap.c"
#include "recv.nonblock.c"
#include "sysconf.c"
#include "windows.c"

HANDLE fileDesHandle (int fd) {
  // The temporary prevents a "cast does not match function type" warning.
  long t;

  t = get_osfhandle (fd);
  return (HANDLE)t;
}

void GC_decommit (void *base, size_t length) {
        if (MLton_Platform_CygwinUseMmap)
                munmap_safe (base, length);
        else
                Windows_decommit (base, length);
}

void *GC_mmapAnon (void *start, size_t length) {
        if (MLton_Platform_CygwinUseMmap)
                return mmapAnon (start, length);
        else
                return Windows_mmapAnon (start, length);
}

void GC_release (void *base, size_t length) {
        if (MLton_Platform_CygwinUseMmap)
                munmap_safe (base, length);
        else
                Windows_release (base);
}

/* ------------------------------------------------- */
/*                      Cygwin                       */
/* ------------------------------------------------- */

C_String_t Cygwin_toFullWindowsPath (NullString8_t path) {
        static char res[MAX_PATH];

        cygwin_conv_to_full_win32_path ((char*)path, &res[0]);
        return (C_String_t)&res[0];
}

/* ------------------------------------------------- */
/*                       Posix                       */
/* ------------------------------------------------- */

void Posix_IO_setbin (C_Fd_t fd) {
        /* cygwin has a different method for working with its fds */
        setmode (fd, O_BINARY);
}

void Posix_IO_settext (C_Fd_t fd) {
        /* cygwin has a different method for working with its fds */
        setmode (fd, O_TEXT);
}

/* ------------------------------------------------- */
/*                      Process                      */
/* ------------------------------------------------- */

/* Cygwin replaces cwait with a call to waitpid.
 * waitpid only works when the process was created by cygwin and there
 * is a secret magical pipe for sending signals and exit statuses over.
 * Screw that. We implement our own cwait using pure win32.
 */
C_Errno_t(C_PId_t) MLton_Process_cwait(C_PId_t pid, Ref(C_Status_t) status) {
        HANDLE h;
        
        h = (HANDLE)pid;
        /* This all works on Win95+ */
        while (1) {
                /* Using an open handle we can get the exit status */
                unless (GetExitCodeProcess (h, (DWORD*)status)) {
                        /* An error probably means the child does not exist */
                        errno = ECHILD;
                        return -1;
                }
                /* Thank you windows API.
                 * I hope no process ever exits with STILL_ACTIVE.
                 * At least most other windows programs have this bug too.
                 */
                if (*(DWORD*)status != STILL_ACTIVE) /* 259 */
                        break;
                /* Wait for h to change state for up to one second.
                 * We don't wait longer b/c there is a race condition
                 * between checking the exit status and calling this method.
                 * By only waiting 1s, no infinite loop can result.
                 */
                WaitForSingleObject (h, 1000);
        }
        /* Cleanup the process handle -- don't call this method again */
        CloseHandle (h);
        return pid;
}
