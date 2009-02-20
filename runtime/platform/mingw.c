#define _GNU_SOURCE

#include "platform.h"

#include "windows.c"
#include "mremap.c"

void *GC_mmapAnon (void *start, size_t length) {
        return Windows_mmapAnon (start, length);
}

void GC_release (void *base, size_t length) {
        Windows_release (base, length);
}

void *GC_extendHead (void *base, size_t length) {
        return Windows_mmapAnon (base, length);
}

void *GC_extendTail (void *base, size_t length) {
        return Windows_extend (base, length);
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
        return (size_t)sysinfo.dwAllocationGranularity;
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

/* Based on notes by Wu Yongwei and IBM:
 *   http://mywebpage.netscape.com/yongweiwutime.htm
 *   http://www.ibm.com/developerworks/library/i-seconds/
 *
 * The basic plan is to get an initial time using GetSystemTime
 * that is good up to ~10ms accuracy. From then on, we compute
 * using deltas with the high-resolution (> microsecond range)
 * performance timers. A 64-bit accumulator holds microseconds
 * since (*nix) epoch. This is good for over 500,000 years before
 * wrap-around becomes a concern. However, we do need to watch
 * out for wrap-around with the QueryPerformanceCounter, because
 * it could be measuring at a higher frequency than microseconds.
 */
int gettimeofday (struct timeval *tv,
                  __attribute__ ((unused)) struct timezone *tz) {
        static LARGE_INTEGER frequency;
        static LARGE_INTEGER baseCounter;
        static LARGE_INTEGER microSeconds; /* static vars start = 0 */

        LARGE_INTEGER deltaCounter;
        LARGE_INTEGER nowMicroSeconds;

        if (microSeconds.QuadPart == 0) {
                FILETIME ft;

                /* tzset prepares the localtime function. I don't
                 * really understand why it's here and not there,
                 * but this has been the case since before svn logs.
                 * So I leave it here to preserve the status-quo.
                 */
                tzset();

                GetSystemTimeAsFileTime (&ft);
                QueryPerformanceCounter(&baseCounter);
                QueryPerformanceFrequency(&frequency);
                if (frequency.QuadPart == 0)
                        die("no high resolution clock");

                microSeconds.LowPart = ft.dwLowDateTime;
                microSeconds.HighPart = ft.dwHighDateTime;
                microSeconds.QuadPart -= EPOCHFILETIME;
                microSeconds.QuadPart /= 10; /* 100ns -> 1ms */
        }

        QueryPerformanceCounter(&deltaCounter);
        deltaCounter.QuadPart -= baseCounter.QuadPart;
        nowMicroSeconds = microSeconds;
        nowMicroSeconds.QuadPart +=
                1000000 * deltaCounter.QuadPart / frequency.QuadPart;

        tv->tv_sec = (long)(nowMicroSeconds.QuadPart / 1000000);
        tv->tv_usec = (long)(nowMicroSeconds.QuadPart % 1000000);

        /* Watch out for wrap-around in the PerformanceCounter.
         * We expect the delta * 1000000 to fit inside a 64 bit integer.
         * To be safe, we will rebase the clock whenever it exceeds 32 bits.
         * We don't want to rebase all the time because it introduces drift.
         */
        if (nowMicroSeconds.HighPart != 0) {
                microSeconds = nowMicroSeconds;
                baseCounter.QuadPart += deltaCounter.QuadPart;
        }

        return 0;
}

/* ------------------------------------------------- */
/*                   MLton.Itimer                    */
/* ------------------------------------------------- */

/* We use the kernel's TimerQueues -- see:
 *  http://msdn.microsoft.com/en-us/library/ms686796(VS.85).aspx
 */

static HANDLE MainThread = NULL;
static HANDLE TimerQueue = NULL;
static HANDLE RealTimer = NULL;
static HANDLE VirtTimer = NULL;
static HANDLE ProfTimer = NULL;
static HANDLE PrioTimer = NULL;
static void (*SIGALRM_handler)(int sig) = SIG_DFL;
static void (*SIGVTAM_handler)(int sig) = SIG_DFL;
static void (*SIGPROF_handler)(int sig) = SIG_DFL;

/* The timer handler is fired in another thread.
 * The idea is to suspend the main thread and resume it once we're done.
 * This will appear more-or-less the same as if a Unix system had received
 * the signal. We will also be firing the handler in the timer thread itself
 * for performance reasons (MLton uses this mechanism to do multi-threading).
 * This means the signal handlers must be fast, which they are since they
 * just mark the signal to be processed later.
 */

static VOID CALLBACK MLton_SIGALRM(__attribute__ ((unused)) PVOID myArg,
                                   __attribute__ ((unused)) BOOLEAN timeout) {
        SuspendThread(MainThread);
        if (SIGALRM_handler == SIG_IGN) {
                /* noop */
        } else if (SIGALRM_handler == SIG_DFL) {
                die("alarm");
        } else {
                (*SIGALRM_handler)(SIGALRM);
        }
        ResumeThread(MainThread);
}
static VOID CALLBACK MLton_SIGVTAM(__attribute__ ((unused)) PVOID myArg,
                                   __attribute__ ((unused)) BOOLEAN timeout) {
        SuspendThread(MainThread);
        if (SIGVTAM_handler == SIG_IGN) {
                /* noop */
        } else if (SIGVTAM_handler == SIG_DFL) {
                die("vtalarm");
        } else {
                (*SIGVTAM_handler)(SIGVTALRM);
        }
        ResumeThread(MainThread);
}
static VOID CALLBACK MLton_SIGPROF(__attribute__ ((unused)) PVOID myArg,
                                   __attribute__ ((unused)) BOOLEAN timeout) {
        SuspendThread(MainThread);
        if (SIGPROF_handler == SIG_IGN) {
                /* noop */
        } else if (SIGPROF_handler == SIG_DFL) {
                die("sigprof");
        } else {
                (*SIGPROF_handler)(SIGPROF);
        }
        ResumeThread(MainThread);
}

static void CALLBACK fixPriority(__attribute__ ((unused)) PVOID myArg,
                                 __attribute__ ((unused)) BOOLEAN timeout) {
        SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_ABOVE_NORMAL);
        DeleteTimerQueueTimer(TimerQueue, PrioTimer, NULL);
}

static int MLTimer(HANDLE *timer,
                   const struct itimerval *value,
                   WAITORTIMERCALLBACK callback) {
        DWORD DueTime, Period;

        /* Initialize the TimerQueue */
        if (MainThread == 0) {
                /* This call improves the resolution of the scheduler from
                 * 16ms to about 2ms in my testing. Sadly it requires winmm.
                 */
                timeBeginPeriod(1);

                TimerQueue = CreateTimerQueue();
                if (TimerQueue == NULL) { errno = ENOMEM; return -1; }

                /* We need to get the TimerQueue to have higher priority.
                 * From my testing, if it has the same priority as the main
                 * thread and the main thread is busy, your best resolution
                 * is a terribly slow 188ms. By boosting the priority of the
                 * timer thread to ABOVE_NORMAL, I've gotten down to 2ms.
                 */
                CreateTimerQueueTimer(&PrioTimer, TimerQueue, fixPriority,
                                      0, 1, 0, WT_EXECUTEINTIMERTHREAD);

                /* We need a handle to the main thread usable by the timer
                 * thread. GetCurrentThread() is a self-reference so we need
                 * to copy it to a new handle for it to work in other threads.
                 */
                DuplicateHandle(GetCurrentProcess(), /* source process */
                                GetCurrentThread(),  /* source handle  */
                                GetCurrentProcess(), /* target process */
                                &MainThread,         /* target handle  */
                                0,                   /* access (ignored) */
                                FALSE,               /* not inheritable */
                                DUPLICATE_SAME_ACCESS);

                if (MainThread == 0) die("Cannot get handle to initial thread");
        }

        /* Windows uses ms accuracy for TimerQueues */
        DueTime = value->it_value.tv_sec * 1000
                + (value->it_value.tv_usec + 999) / 1000;
        Period  = value->it_interval.tv_sec * 1000
                + (value->it_interval.tv_usec + 999) / 1000;

        if (timer != NULL) {
                DeleteTimerQueueTimer(TimerQueue, *timer, NULL);
                *timer = NULL;
        }

        if (DueTime == 0) {
                return 0;
        }

        if (!CreateTimerQueueTimer(
                timer,       /* output: created timer */
                TimerQueue,  /* The queue which holds the timers */
                callback,    /* Invoked on timer events */
                0,           /* myArg for the callback */
                DueTime,     /* Must be non-zero => time till first event */
                Period,      /* Time till the event repeats (forever) */
                WT_EXECUTEINTIMERTHREAD)) { /* Don't use a thread pool */
                 errno = ENOMEM;
                 return -1;
        }

        return 0;
}

int setitimer (int which,
               const struct itimerval *value,
               struct itimerval *ovalue) {
        if (ovalue != 0) die("setitimer doesn't support retrieving old state");

        switch (which) {
        case ITIMER_REAL: return MLTimer(&RealTimer, value, &MLton_SIGALRM);
        case ITIMER_VIRT: return MLTimer(&VirtTimer, value, &MLton_SIGVTAM);
        case ITIMER_PROF: return MLTimer(&ProfTimer, value, &MLton_SIGPROF);
        default:          errno = EINVAL; return -1;
        }

}

static void catcher(__attribute__ ((unused)) int sig) {
        CONTEXT context;
        context.ContextFlags = CONTEXT_CONTROL;

        GetThreadContext(MainThread, &context);
#if defined(__i386__)
        GC_handleSigProf((code_pointer) context.Eip);
#elif defined(__x86_64__)
        GC_handleSigProf((code_pointer) context.Rip);
#elif defined(_PPC_)
        GC_handleSigProf((code_pointer) context.Iar);
#elif defined(_ALPHA_)
        GC_handleSigProf((code_pointer) context.Fir);
#elif defined(MIPS)
        GC_handleSigProf((code_pointer) context.Fir);
#elif defined(ARM)
        GC_handleSigProf((code_pointer) context.Pc);
#else
#error Profiling handler is missing for this architecture
#endif
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = 0;
        sa->sa_handler = (_sig_func_ptr)&catcher;
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
                        GetMappedFileNameA (GetCurrentProcess(),
                                            pMem, fname, MAX_PATH);
                        UnmapViewOfFile (pMem);
                }
                CloseHandle (fhmap);
        }
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
        die ("getlogin not implemented");
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
#ifdef _WIN64
#define mingw_name "MINGW64"
#else
#define mingw_name "MINGW32"
#endif
        sprintf (buf->sysname, "%s_%s-%d.%d", mingw_name,
                os, (int)osv.dwMajorVersion, (int)osv.dwMinorVersion);
}

int uname (struct utsname *buf) {
        MLton_initSockets(); /* needed for gethostname */
        setMachine (buf);
        setSysname (buf);
        unless (0 == gethostname (buf->nodename, sizeof (buf->nodename))) {
                strcpy (buf->nodename, "unknown");
        }
#ifdef _WIN64
        sprintf (buf->release, "%d", MINGW64_VERSION_MINOR);
        sprintf (buf->version, "%d", MINGW64_VERSION_MAJOR);
#else
        sprintf (buf->release, "%d", __MINGW32_MINOR_VERSION);
        sprintf (buf->version, "%d", __MINGW32_MAJOR_VERSION);
#endif
        return 0;
}

/* ------------------------------------------------- */
/*                   Posix.Process                   */
/* ------------------------------------------------- */

int alarm (int secs) {
        struct itimerval new;
        new.it_interval.tv_usec = 0;
        new.it_interval.tv_sec = 0;
        new.it_value.tv_usec = 0;
        new.it_value.tv_sec = secs;
        return setitimer(ITIMER_REAL, &new, 0);
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
        _sig_func_ptr old;

        if (signum < 0 or signum >= NSIG) {
                errno = EINVAL;
                return -1;
        }

        switch (signum) {
        case SIGKILL:
        case SIGSTOP:
                errno = EINVAL;
                return -1;
        case SIGALRM:
                old = SIGALRM_handler;
                if (newact) SIGALRM_handler = newact->sa_handler;
                break;
        case SIGVTALRM:
                old = SIGVTAM_handler;
                if (newact) SIGVTAM_handler = newact->sa_handler;
                break;
        case SIGPROF:
                old = SIGPROF_handler;
                if (newact) SIGPROF_handler = newact->sa_handler;
                break;
        default:
                old = signal (signum, newact?newact->sa_handler:0);
                if (!newact) signal (signum, old);
                break;
        }

        if (oldact)
                oldact->sa_handler = old;
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
/*                        MinGW                      */
/* ------------------------------------------------- */

C_Size_t MinGW_getTempPath(C_Size_t buf_size, Array(Char8_t) buf) {
        return GetTempPath(buf_size, (char*)buf);
}
