#define _GNU_SOURCE

#include "platform.h"

#include "mkdir2.c"
#include "mmap.c"
#include "recv.nonblock.c"
#include "windows.c"
#include "mremap.c"

/* 
 * The sysconf(_SC_PAGESIZE) is the necessary alignment for using
 * mmap.  Windows has another notion of page size (that corresponds to
 * physical page size?).  Just to be safe, we take the least common
 * multiple of the sysconf and Windows notions of page size.
 *
 * Since sysconf(_SC_PAGESIZE) might not correspond to the physical
 * page size, we can't use sysconf(_SC_PHYS_PAGES) to get physical
 * memory.  So, use the Windows function.
 * 
 * See: http://cygwin.com/ml/cygwin/2006-06/msg00341.html
 */ 
static size_t GC_pageSize_sysconf (void) {
  SYSTEM_INFO sysinfo;
  long int pageSize;

  pageSize = sysconf (_SC_PAGESIZE);
  GetSystemInfo(&sysinfo);
  
  /* MLton_Platform_CygwinUseMmap is not set when this is called.
   * Assume the worst; choose the larger allocation unit.
   */
  if ((size_t)pageSize < (size_t)sysinfo.dwAllocationGranularity)
    return (size_t)sysinfo.dwAllocationGranularity;
  else
    return (size_t)pageSize;
}

static size_t GC_pageSize_windows (void) {
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  return (size_t)sysinfo.dwPageSize;
}

size_t GC_pageSize (void) {
  size_t pageSize_sysconf = GC_pageSize_sysconf ();
  size_t pageSize_windows = GC_pageSize_windows ();

  size_t a = pageSize_sysconf;
  size_t b = pageSize_windows;
  size_t t;
  while (b != 0) {
    t = b;
    b = a % b;
    a = t;
  }
  size_t gcd = a;

  size_t lcm = (pageSize_sysconf / gcd) * pageSize_windows;
  
  return lcm;
}

uintmax_t GC_physMem (void) {
  MEMORYSTATUS memstat;

  memstat.dwLength = sizeof(memstat);
  GlobalMemoryStatus(&memstat);
  return (uintmax_t)memstat.dwTotalPhys;
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
                Windows_release (base, length);
}

void* GC_extendHead (void *base, size_t length) {
        if (MLton_Platform_CygwinUseMmap)
                return mmapAnon (base, length);
        else
                return Windows_mmapAnon (base, length);
}

void* GC_extendTail (void *base, size_t length) {
        if (MLton_Platform_CygwinUseMmap)
                return mmapAnon (base, length);
        else
                return Windows_extend (base, length);
}

HANDLE fileDesHandle (int fd) {
  // The temporary prevents a "cast does not match function type" warning.
  long t;

  t = get_osfhandle (fd);
  return (HANDLE)t;
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

C_Errno_t(C_PId_t) MLton_Process_cwait (C_PId_t pid, Ref(C_Status_t) status) {
  HANDLE h;

  h = (HANDLE)pid;
  /* -1 on error, the casts here are due to bad types on both sides */
  return cwait ((int*)status, (pid_t)h, 0);
}

/* 20070822, fluet: The following 'pure win32' implementation of cwait
 * no longer works on recent Cygwin versions.  It always takes the
 * {errno = ECHILD; return -1} branch, even when the child process
 * exists.
 */

/* Cygwin replaces cwait with a call to waitpid.
 * waitpid only works when the process was created by cygwin and there
 * is a secret magical pipe for sending signals and exit statuses over.
 * Screw that. We implement our own cwait using pure win32.
 */
/* C_Errno_t(C_PId_t) MLton_Process_cwait(C_PId_t pid, Ref(C_Status_t) status) { */
/*         HANDLE h; */

/*         h = (HANDLE)pid; */
/*         /\* This all works on Win95+ *\/ */
/*         while (1) { */
/*                 /\* Using an open handle we can get the exit status *\/ */
/*                 unless (GetExitCodeProcess (h, (DWORD*)status)) { */
/*                         /\* An error probably means the child does not exist *\/ */
/*                         errno = ECHILD; */
/*                         return -1; */
/*                 } */
/*                 /\* Thank you windows API. */
/*                  * I hope no process ever exits with STILL_ACTIVE. */
/*                  * At least most other windows programs have this bug too. */
/*                  *\/ */
/*                 if (*(DWORD*)status != STILL_ACTIVE) /\* 259 *\/ */
/*                         break; */
/*                 /\* Wait for h to change state for up to one second. */
/*                  * We don't wait longer b/c there is a race condition */
/*                  * between checking the exit status and calling this method. */
/*                  * By only waiting 1s, no infinite loop can result. */
/*                  *\/ */
/*                 WaitForSingleObject (h, 1000); */
/*         } */
/*         /\* Cleanup the process handle -- don't call this method again *\/ */
/*         CloseHandle (h); */
/*         return pid; */
/* } */
