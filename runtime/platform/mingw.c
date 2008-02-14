#define _GNU_SOURCE

#include "platform.h"

#include "windows.c"

void GC_decommit (void *base, size_t length) {
        Windows_decommit (base, length);
}

void *GC_mremap (void *base, size_t old, size_t new) {
        return Windows_mremap (base, old, new);
}

void *GC_mmapAnon (void *start, size_t length) {
        return Windows_mmapAnon (start, length);
}

void GC_release (void *base, 
                 __attribute__ ((unused)) size_t length) {
        Windows_release (base);
}

uintmax_t GC_physMem (void) {
#ifdef _WIN64
        MEMORYSTATUSEX memstat;
        memstat.dwLength = sizeof(memstat);
        GlobalMemoryStatusEx(&memstat);
        return (uintmax_t)memstat.ullTotalPhys;
#else
        MEMORYSTATUS memstat;
        memstat.dwLength = sizeof(memstat);
        GlobalMemoryStatus(&memstat);
        return (uintmax_t)memstat.dwTotalPhys;
#endif
}

size_t GC_pageSize (void) {
        SYSTEM_INFO sysinfo;
        GetSystemInfo(&sysinfo);
        return (size_t)sysinfo.dwPageSize;
}

HANDLE fileDesHandle (int fd) {
  // The temporary prevents a "cast does not match function type" warning.
  intptr_t t;

  t = _get_osfhandle (fd);
  return (HANDLE)t;
}

int mkstemp (char *template) {
        char file_path[255];
        char file_name[255];
        char templ[4];

        if (0 == GetTempPath (sizeof (file_path), file_path))
                diee ("unable to make temporary file");
        strncpy (templ, template, sizeof (templ) - 1);
        templ[sizeof (templ) - 1] = 0x00;
        if (0 == GetTempFileName (file_path, templ, 0, file_name))
                diee ("unable to make temporary file");
        return _open (file_name, _O_CREAT | _O_RDWR, _S_IREAD | _S_IWRITE);
}

/* ------------------------------------------------- */
/*                       Date                        */
/* ------------------------------------------------- */

#ifndef __GNUC__
#define EPOCHFILETIME (116444736000000000i64)
#else
#define EPOCHFILETIME (116444736000000000LL)
#endif

/* Based on notes by Wu Yongwei: 
 *   http://mywebpage.netscape.com/yongweiwutime.htm 
 */
int mlton_gettimeofday (struct timeval *tv, 
                        __attribute__ ((unused)) struct timezone *tz) {
        FILETIME ft;
        LARGE_INTEGER li;
        __int64 t;
        static bool tzInit = FALSE;

        unless (tzInit) {
                tzInit = TRUE;
                _tzset();
        }
        GetSystemTimeAsFileTime (&ft);
        li.LowPart = ft.dwLowDateTime;
        li.HighPart = ft.dwHighDateTime;
        t = li.QuadPart;
        t -= EPOCHFILETIME;
        t /= 10;
        tv->tv_sec = (long)(t / 1000000);
        tv->tv_usec = (long)(t % 1000000);
        return 0;
}

/* ------------------------------------------------- */
/*                   MLton.Itimer                    */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
int setitimer (__attribute__ ((unused)) int which, 
               __attribute__ ((unused)) const struct itimerval *value, 
               __attribute__ ((unused)) struct itimerval *ovalue) {
        // !!! perhaps use code from alarm?
        die ("setitimer not implemented");
}

/* ------------------------------------------------- */
/*                   MLton.Rlimit                    */
/* ------------------------------------------------- */

static struct rlimit rlimits[RLIM_NLIMITS];

static void initRlimits (void) {
        static int done = FALSE;
        int lim;

        if (done)
                return;
        done = TRUE;
        for (lim = 0; lim < RLIM_NLIMITS; ++lim ) {
                rlimits[lim].rlim_cur = 0;
                rlimits[lim].rlim_max = UINT_MAX;
        }
}

int getrlimit (int resource, struct rlimit *rlp) {
        initRlimits ();
        if (resource < 0 or resource >= RLIM_NLIMITS) {
                errno = EINVAL;
                return -1;
        }
        *rlp = rlimits[resource];
        return 0;
}

int setrlimit (int resource, const struct rlimit *rlp) {
        initRlimits ();
        if (resource < 0 or resource >= RLIM_NLIMITS) {
                errno = EINVAL;
                return -1;
        }
        if (rlp->rlim_cur < rlimits[resource].rlim_max)
                rlimits[resource].rlim_cur = rlp->rlim_cur;
        else {
                errno = EPERM;
                return -1;
        }
        rlimits[resource].rlim_max = rlp->rlim_max;
        return 0;
}

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

/* GetProcessTimes and GetSystemTimeAsFileTime are documented at:
 *   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/getprocesstimes.asp
 */
int getrusage (int who, struct rusage *usage) {
  /* FILETIME has dw{High,Low}DateTime which store the number of
   * 100-nanoseconds since January 1, 1601
   */
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;

  uint64_t user_usecs, kernel_usecs;

  if (who == RUSAGE_CHILDREN) {
    // !!! could use exit_time - creation_time from cwait 
    memset(usage, 0, sizeof(struct rusage));
    return 0;
  }
  
  if (who != RUSAGE_SELF) {
    errno = EINVAL;
    return -1;
  }

  if (GetProcessTimes(GetCurrentProcess(), 
                      &creation_time, &exit_time,
                      &kernel_time, &user_time) == 0) {
    errno = EFAULT;
    return -1;
  }
  
  kernel_usecs = kernel_time.dwHighDateTime;
  kernel_usecs <<= sizeof(kernel_time.dwHighDateTime)*8;
  kernel_usecs |= kernel_time.dwLowDateTime;
  kernel_usecs /= 10;

  user_usecs = user_time.dwHighDateTime;
  user_usecs <<= sizeof(user_time.dwHighDateTime)*8;
  user_usecs |= user_time.dwLowDateTime;
  user_usecs /= 10;

  usage->ru_utime.tv_sec  = user_usecs / 1000000;
  usage->ru_utime.tv_usec = user_usecs % 1000000;
  usage->ru_stime.tv_sec  = kernel_usecs / 1000000;
  usage->ru_stime.tv_usec = kernel_usecs % 1000000;
  return 0;
}

/* ------------------------------------------------- */
/*                       OS.IO                       */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
int poll (__attribute__ ((unused)) struct pollfd *ufds, 
          __attribute__ ((unused)) unsigned int nfds, 
          __attribute__ ((unused)) int timeout) {
        die ("poll not implemented");
}

/* ------------------------------------------------- */
/*                   Posix.FileSys                   */
/* ------------------------------------------------- */

static void GetWin32FileName (int fd, char* fname) {
        HANDLE fh, fhmap;
        DWORD fileSize, fileSizeHi;
        void* pMem = NULL;
        intptr_t tmp;

        tmp = _get_osfhandle (fd);
        fh = (HANDLE)tmp;
        fileSize = GetFileSize (fh, &fileSizeHi);
        fhmap = CreateFileMapping (fh, NULL, PAGE_READONLY, 0, fileSize, NULL);
        if (fhmap) {
            pMem = MapViewOfFile (fhmap, FILE_MAP_READ, 0, 0, 1);
            if (pMem) {
                GetMappedFileNameA (GetCurrentProcess(), pMem, fname, MAX_PATH);
                UnmapViewOfFile (pMem);
            }
            CloseHandle (fhmap);
        }
        return; 
}

int fchmod (int filedes, mode_t mode) {
      char fname[MAX_PATH + 1];

      GetWin32FileName (filedes, fname);
      return _chmod (fname, mode);
}

int fchdir (int filedes) {
      char fname[MAX_PATH + 1];

      GetWin32FileName (filedes, fname);
      return chdir (fname);
}

__attribute__ ((noreturn))
int chown (__attribute__ ((unused)) const char *path, 
           __attribute__ ((unused)) uid_t owner, 
           __attribute__ ((unused)) gid_t group) {
        die ("chown not implemented");
}

__attribute__ ((noreturn))
int fchown (__attribute__ ((unused)) int fd, 
            __attribute__ ((unused)) uid_t owner, 
            __attribute__ ((unused)) gid_t group) {
        die ("fchown not implemented");
}

__attribute__ ((noreturn))
long fpathconf (__attribute__ ((unused)) int filedes, 
                __attribute__ ((unused)) int name) {
        die ("fpathconf not implemented");
}

__attribute__ ((noreturn))
int link (__attribute__ ((unused)) const char *oldpath, 
          __attribute__ ((unused)) const char *newpath) {
        die ("link not implemented");
}

int lstat (const char *file_name, struct stat *buf) {
        /* Win32 doesn't really have links. */
        return stat (file_name, buf);
}

int mkdir2 (const char *pathname, mode_t mode) {
        return mkdir (pathname, mode);
}

__attribute__ ((noreturn))
int mkfifo (__attribute__ ((unused)) const char *pathname, 
            __attribute__ ((unused)) mode_t mode) {
        die ("mkfifo not implemented");
}

__attribute__ ((noreturn))
long pathconf (__attribute__ ((unused)) const char *path, 
               __attribute__ ((unused)) int name) {
        die ("pathconf not implemented");
}

__attribute__ ((noreturn))
int readlink (__attribute__ ((unused)) const char *path, 
              __attribute__ ((unused)) char *buf, 
              __attribute__ ((unused)) size_t bufsiz) {
        die ("readlink not implemented");
}

__attribute__ ((noreturn))
int symlink (__attribute__ ((unused)) const char *oldpath, 
             __attribute__ ((unused)) const char *newpath) {
        die ("symlink not implemented");
}

int truncate (const char *path, off_t len) {
  int fd;

  if ((fd = open(path, O_RDWR)) == -1)
    return -1;
  if (ftruncate(fd, len) < 0) {
    close(fd);
    return -1;
  }
  close(fd);
  return 0;
}


/* ------------------------------------------------- */
/*                     Posix.IO                      */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
int fcntl (__attribute__ ((unused)) int fd, 
           __attribute__ ((unused)) int cmd, 
           ...) {
        die ("fcntl not implemented");
}

int fsync (int fd) {
        return _commit (fd);
}

int pipe (int filedes[2]) {
        HANDLE read_h;
        HANDLE write_h;

        /* We pass no security attributes (0), so the current policy gets
         * inherited. The pipe is set to NOT stay open in child processes.
         * This will be corrected using DuplicateHandle in create()
         * The 4k buffersize is choosen b/c that's what linux uses.
         */
        if (!CreatePipe(&read_h, &write_h, 0, 4096)) {
                errno = ENOMEM; /* fake errno: out of resources */
                return -1;
        }
        /* This requires Win98+
         * Choosing text/binary mode is defered till a later setbin/text call
         */
        filedes[0] = _open_osfhandle((intptr_t)read_h,  _O_RDONLY);
        filedes[1] = _open_osfhandle((intptr_t)write_h, _O_WRONLY);
        if (filedes[0] == -1 or filedes[1] == -1) {
                if (filedes[0] == -1) 
                        CloseHandle(read_h); 
                else    close(filedes[0]);
                if (filedes[1] == -1) 
                        CloseHandle(write_h);
                else    close(filedes[1]);

                errno = ENFILE;
                return -1;
        }
        return 0;
}

/* ------------------------------------------------- */
/*                   Posix.ProcEnv                   */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
char *ctermid (__attribute__ ((unused)) char* s) {
        die ("*ctermid not implemented");
}

__attribute__ ((noreturn))
gid_t getegid (void) {
        die ("getegid not implemented");
}

__attribute__ ((noreturn))
uid_t geteuid (void) {
        die ("geteuid not implemented");
}

__attribute__ ((noreturn))
gid_t getgid (void) {
        die ("getgid not implemented");
}

__attribute__ ((noreturn))
int getgroups (__attribute__ ((unused)) int size, 
               __attribute__ ((unused)) gid_t list[]) {
        die ("getgroups not implemented");
}

__attribute__ ((noreturn))
char *getlogin (void) {
        die ("*getlogin not implemented");
}

__attribute__ ((noreturn))
pid_t getpgid(__attribute__ ((unused)) pid_t pid) {
        die ("getpgid not implemented");
}

__attribute__ ((noreturn))
pid_t getpgrp(void) {
        die ("getpgrp not implemented");
}

__attribute__ ((noreturn))
pid_t getppid (void) {
        die ("getppid not implemented");
}

__attribute__ ((noreturn))
uid_t getuid (void) {
        die ("getuid not implemented");
}

int setenv (const char *name, const char *value, int overwrite) {
        /* We could use _putenv, but then we'd need a temporary buffer for
         * use to concat name=value. 
         */
        if (not overwrite and getenv (name)) {
                errno = EEXIST;
                return -1; /* previous mingw setenv was buggy and returned 0 */
        }

        if (SetEnvironmentVariable (name, value)) {
                errno = ENOMEM; /* this happens often in Windows.. */
                return -1;
        }

        return 0;
}

__attribute__ ((noreturn))
int setgid (__attribute__ ((unused)) gid_t gid) {
        die ("setgid not implemented");
}

__attribute__ ((noreturn))
int setgroups (__attribute__ ((unused)) size_t size, 
               __attribute__ ((unused)) const gid_t *list) {
        die ("setgroups not implemented");
}

__attribute__ ((noreturn))
int setpgid (__attribute__ ((unused)) pid_t pid, 
             __attribute__ ((unused)) pid_t pgid) {
        die ("setpgid not implemented");
}

__attribute__ ((noreturn))
pid_t setsid (void) {
        die ("setsid not implemented");
}

__attribute__ ((noreturn))
int setuid (__attribute__ ((unused)) uid_t uid) {
        die ("setuid not implemented");
}

__attribute__ ((noreturn))
long sysconf (__attribute__ ((unused)) int name) {
        die ("sysconf not implemented");
}

__attribute__ ((noreturn))
clock_t times (__attribute__ ((unused)) struct tms *buf) {
        die ("times not implemented");
}

__attribute__ ((noreturn))
char *ttyname (__attribute__ ((unused)) int desc) {
        die ("*ttyname not implemented");
}

static void setMachine (struct utsname *buf) {
        int level;
        const char* platform = "unknown";
        SYSTEM_INFO si;

        GetSystemInfo (&si);
        level = si.dwProcessorType;
        switch (si.wProcessorArchitecture) {
        case PROCESSOR_ARCHITECTURE_INTEL:
                if (level < 3) level = 3;
                if (level > 6) level = 6;
                platform = "i%d86"; 
                break;
        case PROCESSOR_ARCHITECTURE_IA64:    platform = "ia64";    break;
        case PROCESSOR_ARCHITECTURE_AMD64:   platform = "amd64";   break; 
        case PROCESSOR_ARCHITECTURE_PPC:     platform = "ppc";     break;
        case PROCESSOR_ARCHITECTURE_ALPHA:   platform = "alpha";   break;
        case PROCESSOR_ARCHITECTURE_MIPS:    platform = "mips";    break;
        case PROCESSOR_ARCHITECTURE_ARM:     platform = "arm";     break;
        case PROCESSOR_ARCHITECTURE_ALPHA64: platform = "alpha64"; break;
        /* SHX? MSIL? IA32_ON_WIN64? */
        default: platform = "unknown"; break;
        }
        sprintf (buf->machine, platform, level);
}

static void setSysname (struct utsname *buf) {
        OSVERSIONINFO osv;
        const char* os = "??";

        osv.dwOSVersionInfoSize = sizeof (osv);
        GetVersionEx (&osv);
        switch (osv.dwPlatformId) {
        case VER_PLATFORM_WIN32_NT:
                if (osv.dwMinorVersion == 0) {
                        if (osv.dwMajorVersion <= 4)    os = "NT";
                        else                            os = "2000";
                } else if (osv.dwMinorVersion <= 1)     os = "XP";
                else if (osv.dwMinorVersion <= 2)       os = "2003";
                else                                    os = "NTx";
                break;
        case VER_PLATFORM_WIN32_WINDOWS:
                if (osv.dwMinorVersion == 0)            os = "95";
                else if (osv.dwMinorVersion < 90)       os = "98";
                else if (osv.dwMinorVersion == 90)      os = "Me";
                else                                    os = "9X";
                break;
        case VER_PLATFORM_WIN32s:
                os = "31"; /* aka DOS + Windows 3.1 */
                break;
        default:
                os = "unknown";
                break;
        }
        sprintf (buf->sysname, "MINGW32_%s-%d.%d",
                os, (int)osv.dwMajorVersion, (int)osv.dwMinorVersion);
}

int uname (struct utsname *buf) {
        setMachine (buf);
        unless (0 == gethostname (buf->nodename, sizeof (buf->nodename))) {
                strcpy (buf->nodename, "unknown");
        }
        sprintf (buf->release, "%d", 0); //__MINGW32_MINOR_VERSION);
        setSysname (buf);
        sprintf (buf->version, "%d", 0); //__MINGW32_MAJOR_VERSION);
        return 0;
}

/* ------------------------------------------------- */
/*                   Posix.Process                   */
/* ------------------------------------------------- */

static UINT_PTR curr_timer = 0;
static int curr_timer_dur = 0;
static LARGE_INTEGER timer_start_val;


static VOID CALLBACK alarm_signalled(__attribute__ ((unused)) HWND window,
                                     __attribute__ ((unused)) UINT message,
                                     __attribute__ ((unused)) UINT_PTR timer_id,
                                     __attribute__ ((unused)) DWORD timestamp) {
    printf("Timer fired\n");
}

/*
 * Win32 alarm implementation
 */
int alarm (int secs) {
        LARGE_INTEGER timer_end_val, frequency;
        int remaining = 0;
        long elapse = secs * 1000;      /* win32 uses usecs */

        /* Unsetting the alarm */
        if (secs == 0 && curr_timer == 0) {
            return 0;
        }
        if (curr_timer != 0) {
                KillTimer(0, curr_timer);
                QueryPerformanceCounter(&timer_end_val);
                QueryPerformanceFrequency(&frequency);
                if (frequency.QuadPart != 0) {
                        remaining = curr_timer_dur - ((int)(timer_end_val.QuadPart
                                - timer_start_val.QuadPart)/frequency.QuadPart);
                        if (remaining < 0) {
                                remaining = 0;
                        }
                }

                timer_start_val.QuadPart = 0;
                curr_timer_dur = 0;
                curr_timer = 0;
        }
        if (secs != 0) {
                /* Otherwise, set a timer */
                curr_timer = SetTimer(0, 0, elapse, alarm_signalled);
                QueryPerformanceCounter(&timer_start_val);
                curr_timer_dur = secs;
        }
        return remaining;
}

__attribute__ ((noreturn))
int fork (void) {
        die ("fork not implemented");
}


__attribute__ ((noreturn))
int kill (__attribute__ ((unused)) pid_t pid, 
          __attribute__ ((unused)) int sig) {
        die ("kill not implemented");
}

int nanosleep (const struct timespec *req, struct timespec *rem) {
        Sleep (req->tv_sec * 1000 + (req->tv_nsec + 999999) / 1000000);
        rem->tv_nsec = 0;
        rem->tv_sec = 0;
        return 0;
}

__attribute__ ((noreturn))
int pause (void) {
        die ("pause not implemented");
}

unsigned int sleep (unsigned int seconds) {
        Sleep (seconds * 1000);
        return 0;
}

__attribute__ ((noreturn))
pid_t wait (__attribute__ ((unused)) int *status) {
        die ("wait not implemented");
}

__attribute__ ((noreturn))
pid_t waitpid (__attribute__ ((unused)) pid_t pid, 
               __attribute__ ((unused)) int *status, 
               __attribute__ ((unused)) int options) {
        die ("waitpid not implemented");
}

/* ------------------------------------------------- */
/*                      Signals                      */
/* ------------------------------------------------- */

int sigaction (int signum, 
                        const struct sigaction *newact,
                        struct sigaction *oldact) {

        struct sigaction oa;

        if (signum < 0 or signum >= NSIG) {
                errno = EINVAL;
                return -1;
        }
        if (newact) {
                if (signum == SIGKILL or signum == SIGSTOP) {
                        errno = EINVAL;
                        return -1;
                }
                oa.sa_handler = signal (signum, newact->sa_handler);
        }
        if (oldact)
                oldact->sa_handler = oa.sa_handler;
        return 0;
}

int sigaddset (sigset_t *set, const int signum) {
        if (signum < 0 or signum >= NSIG) {
                errno = EINVAL;
                return -1;
        }
        *set |= SIGTOMASK (signum);
        return 0;
}

int sigdelset (sigset_t *set, const int signum) {
        if (signum < 0 or signum >= NSIG) {
                errno = EINVAL;
                return -1;
        }
        *set &= ~SIGTOMASK (signum);
        return 0;
}

int sigemptyset (sigset_t *set) {
        *set = (sigset_t) 0;
        return 0;
}

int sigfillset (sigset_t *set) {
        *set = ~((sigset_t) 0);
        return 0;
}

int sigismember (const sigset_t *set, const int signum) {
        if (signum < 0 or signum >= NSIG) {
                errno = EINVAL;
                return -1;
        }
        return (*set & SIGTOMASK(signum)) ? 1 : 0;
}


/* With a bit of work and a redirected signal() function, we could
 * probably emulate these methods properly. AtM blocking is a lie.
 */
static sigset_t signals_blocked = 0;
static sigset_t signals_pending = 0;

int sigpending (sigset_t *set) {
        *set = signals_pending;
        return 0;
}

int sigprocmask (int how, const sigset_t *set, sigset_t *oldset) {
        if (oldset) {
                *oldset = signals_blocked;
        }
        if (set) {
                sigset_t newmask = signals_blocked;

                switch (how) {
                        case SIG_BLOCK:
                                /* add set to current mask */
                                newmask |= *set;
                        break;
                        case SIG_UNBLOCK:
                                /* remove set from current mask */
                                newmask &= ~*set;
                        break;
                        case SIG_SETMASK:
                                /* just set it */
                                newmask = *set;
                        break;
                        default:
                                return -1;
                }

                signals_blocked = newmask;
        }
        return 0;
}

__attribute__ ((noreturn))
int sigsuspend (__attribute__ ((unused)) const sigset_t *mask) {
        die("sigsuspend is unimplemented, but could be hacked in if needed");
}

/* ------------------------------------------------- */
/*                     Posix.IO                      */
/* ------------------------------------------------- */

void Posix_IO_setbin (C_Fd_t fd) {
        _setmode (fd, _O_BINARY);
}

void Posix_IO_settext (C_Fd_t fd) {
        _setmode (fd, _O_TEXT);
}

/* ------------------------------------------------- */
/*                Posix.SysDB.Passwd                 */
/* ------------------------------------------------- */

#define INFO_LEVEL 3
static LPUSER_INFO_3 usrData = NULL;

static struct passwd passwd;

__attribute__ ((noreturn))
struct group *getgrgid (__attribute__ ((unused)) gid_t gid) {
        die ("getgrgid not implemented");
}

__attribute__ ((noreturn))
struct group *getgrnam (__attribute__ ((unused)) const char *name) {
        die ("getgrnam not implemented");
}

struct passwd *getpwnam (__attribute__ ((unused)) const char *name) {
        return NULL;
//      unless (NERR_Success == 
//                      NetUserGetInfo (NULL, (LPCWSTR)name, INFO_LEVEL, 
//                                      (LPBYTE*)&usrData))
//              return NULL;
        passwd.pw_dir = (char*)usrData->usri3_home_dir;
        passwd.pw_gid = usrData->usri3_primary_group_id;
        passwd.pw_name = (char*)usrData->usri3_name;
        passwd.pw_shell = (char*)usrData->usri3_script_path;
        passwd.pw_uid = usrData->usri3_user_id;
        return &passwd;
}

__attribute__ ((noreturn))
struct passwd *getpwuid (__attribute__ ((unused)) uid_t uid) {
        die ("getpwuid not implemented");
}

/* ------------------------------------------------- */
/*                     Posix.TTY                     */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
speed_t cfgetispeed (__attribute__ ((unused)) struct termios *termios_p) {
        die ("cfgetispeed not implemented");
}

__attribute__ ((noreturn))
speed_t cfgetospeed (__attribute__ ((unused)) struct termios *termios_p) {
        die ("cfgetospeed not implemented");
}

__attribute__ ((noreturn))
int cfsetispeed (__attribute__ ((unused)) struct termios *termios_p, 
                 __attribute__ ((unused)) speed_t speed) {
        die ("cfsetispeed not implemented");
}

__attribute__ ((noreturn))
int cfsetospeed (__attribute__ ((unused)) struct termios *termios_p, 
                 __attribute__ ((unused)) speed_t speed) {
        die ("cfsetospeed not implemented");
}

__attribute__ ((noreturn))
int tcdrain (__attribute__ ((unused)) int fd) {
        die ("tcdrain not implemented");
}

__attribute__ ((noreturn))
int tcflow (__attribute__ ((unused)) int fd, 
            __attribute__ ((unused)) int action) {
        die ("tcflow not implemented");
}

__attribute__ ((noreturn))
int tcflush (__attribute__ ((unused)) int fd, 
             __attribute__ ((unused)) int queue_selector) {
        die ("tcflush not implemented");
}

__attribute__ ((noreturn))
int tcgetattr (__attribute__ ((unused)) int fd, 
               __attribute__ ((unused)) struct termios *termios_p) {
        die ("tcgetattr not implemented");
}

__attribute__ ((noreturn))
pid_t tcgetpgrp (__attribute__ ((unused)) int fd) {
        die ("tcgetpgrp not implemented");
}

__attribute__ ((noreturn))
int tcsendbreak (__attribute__ ((unused)) int fd, 
                 __attribute__ ((unused)) int duration) {
        die ("tcsendbreak not implemented");
}

__attribute__ ((noreturn))
int tcsetattr (__attribute__ ((unused)) int fd, 
               __attribute__ ((unused)) int optional_actions, 
               __attribute__ ((unused)) struct termios *termios_p) {
        die ("tcsetattr not implemented");
}

__attribute__ ((noreturn))
int tcsetpgrp (__attribute__ ((unused)) int fd, 
               __attribute__ ((unused)) pid_t pgrpid) {
        die ("tcsetpgrp not implemented");
}

/* ------------------------------------------------- */
/*                      Process                      */
/* ------------------------------------------------- */

C_PId_t MLton_Process_cwait (C_PId_t pid, Pointer status) {
        HANDLE h;

        h = (HANDLE)pid;
        /* -1 on error, the casts here are due to bad types on both sides */
        return _cwait ((int*)status, (_pid_t)h, 0);
}

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
int ioctl (__attribute__ ((unused)) int d, 
           __attribute__ ((unused)) int request, 
           ...) {
        die ("ioctl not implemented");
}

__attribute__ ((noreturn))
int socketpair (__attribute__ ((unused)) int d, 
                __attribute__ ((unused)) int type, 
                __attribute__ ((unused)) int protocol, 
                __attribute__ ((unused)) int sv[2]) {
        die ("socketpair not implemented");
}

void MLton_initSockets (void) {
        static Bool isInitialized = FALSE;
        WORD version;
        WSADATA wsaData;

        unless (isInitialized) {
                isInitialized = TRUE;
                version = MAKEWORD (2,2);
                WSAStartup (version, &wsaData);
        }
}

/* ------------------------------------------------- */
/*                      Syslog                       */
/* ------------------------------------------------- */

static const char* logident = "<unknown>";
static int logopt = LOG_PERROR;
static int logfacility = LOG_LOCAL0;

void openlog(const char* ident, int opt, int facility) {
  logident = ident;
  logopt = opt;
  logfacility = facility;
}

void closelog(void) {
}

void syslog(int priority, __attribute__ ((unused)) const char* fmt, const char* msg) {
  static const char* severity[] = {
    "debug", 
    "informational", 
    "notice", 
    "warning", 
    "error", 
    "CRITICAL", 
    "ALERT", 
    "EMERGENCY"
  };

  if (priority < 0) priority = LOG_DEBUG;
  if (priority > LOG_EMERG) priority = LOG_EMERG;


  /* !!! Use ReportEvent to log with windows */

  if ((logopt & LOG_PERROR) != 0) {
    if ((logopt & LOG_PID) != 0)
      fprintf(stderr, "%s(%d): %s: %s\n", logident, getpid(), severity[priority], msg);
    else
      fprintf(stderr, "%s: %s: %s\n", logident, severity[priority], msg);
  }
}

/* ------------------------------------------------- */
/*                      libdl                        */
/* ------------------------------------------------- */

static DWORD dlerror_last = ERROR_SUCCESS;
/* This is for emulating the ugly stateful behavior of dlerror. */

static HMODULE dl_main_module = NULL;
/* Handle to the main module returned by GetModuleHandle(NULL).  It is
 * assumed that the main module isn't freed during the lifetime of the
 * process.
 */

void *dlopen(const char *filename, __attribute__ ((unused)) int flag_IGNORED) {
        if (!filename) {
                if (!dl_main_module)
                        dl_main_module = GetModuleHandle(NULL);

                if (!dl_main_module)
                        dlerror_last = GetLastError();

                return dl_main_module;
        }

        {
                HMODULE result = LoadLibrary(filename);

                if (!result)
                        dlerror_last = GetLastError();

                return result;
        }
}

const char *dlerror(void) {
        if (ERROR_SUCCESS == dlerror_last) {
                return NULL;
        } else {
                static char buffer[256];

                if (!FormatMessage(FORMAT_MESSAGE_IGNORE_INSERTS |
                                   FORMAT_MESSAGE_FROM_SYSTEM,
                                   NULL, dlerror_last, 0,
                                   buffer, sizeof(buffer),
                                   NULL))
                        snprintf(buffer, sizeof(buffer),
                                 "Failed to format error message");

                dlerror_last = ERROR_SUCCESS;

                return buffer;
        }
}

void *dlsym(void *void_hmodule, const char *symbol) {
        HMODULE hmodule = void_hmodule;

        if (!hmodule) {
                dlerror_last = ERROR_INVALID_HANDLE;
                return NULL;
        }

        {
                void* result = (void*)GetProcAddress(hmodule, symbol);

                if (!result)
                        dlerror_last = GetLastError();

                return result;
        }
}

int dlclose(void *void_hmodule) {
        HMODULE hmodule = void_hmodule;

        if (!hmodule || hmodule == dl_main_module)
                return 0;

        {
                int result = !FreeLibrary(hmodule);

                if (result)
                        dlerror_last = GetLastError();

                return result;
        }
}

/* ------------------------------------------------- */
/*                        MinGW                      */
/* ------------------------------------------------- */

C_Size_t MinGW_getTempPath(C_Size_t buf_size, Array(Char8_t) buf) {
        return GetTempPath(buf_size, (char*)buf);
}
