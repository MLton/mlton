#define _GNU_SOURCE

#include "platform.h"

#include "platform/windows.c"
#include "platform/mremap.c"

void *GC_mmapAnon (void *start, size_t length) {
        return Windows_mmapAnon (start, length);
}

void *GC_mmapAnonFlags (void *start, size_t length, __attribute__ ((unused)) int flags) {
        return GC_mmapAnon(start, length);
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

/* The basic plan is to get an initial time using GetSystemTime
 * that is good up to ~10ms accuracy. From then on, we compute
 * using deltas with the high-resolution (> microsecond range)
 * performance timers. A 64-bit accumulator holds microseconds
 * since (*nix) epoch. This is good for over 500,000 years before
 * wrap-around becomes a concern.
 *
 * However, we might need to watch out for wrap-around with the
 * QueryPerformanceCounter, because it could be measuring at a higher
 * frequency than microseconds.
 *
 * This function only strives to allow a program to run for
 * 100 years without being restarted.
 */
int gettimeofday (struct timeval *tv,
                  __attribute__ ((unused)) struct timezone *tz) {
        static LARGE_INTEGER frequency;        /* ticks/second */
        static LARGE_INTEGER baseCounter;      /* ticks since last rebase */
        static LARGE_INTEGER baseMicroSeconds; /* unix time at last rebase */

        LARGE_INTEGER nowCounter;
        LARGE_INTEGER deltaCounter;
        LARGE_INTEGER nowMicroSeconds;
        double deltaMicroseconds;

        /* This code is run the first time gettimeofday is called. */
        if (frequency.QuadPart == 0) {
                FILETIME ft;
                /* tzset prepares the localtime function. I don't
                 * really understand why it's here and not there,
                 * but this has been the case since before svn logs.
                 * So I leave it here to preserve the status-quo.
                 */
                tzset();

                QueryPerformanceCounter(&baseCounter);
                QueryPerformanceFrequency(&frequency);
                if (frequency.QuadPart == 0)
                        die("no high resolution clock");

                GetSystemTimeAsFileTime (&ft);
                baseMicroSeconds.LowPart = ft.dwLowDateTime;
                baseMicroSeconds.HighPart = ft.dwHighDateTime;
                baseMicroSeconds.QuadPart -= EPOCHFILETIME;
                baseMicroSeconds.QuadPart /= 10; /* 100ns -> 1ms */
        }
        
        /* Use the high res counter ticks to calculate the delta. 
         * A double has 52+1 bits of precision. This means it can fit
         * deltas of up to 9007199254 seconds, or 286 years. We could
         * rebase before an overflow, but 286 is already > 100.
         */
        QueryPerformanceCounter(&nowCounter);
        deltaCounter.QuadPart = nowCounter.QuadPart - baseCounter.QuadPart;
        deltaMicroseconds = deltaCounter.QuadPart;
        deltaMicroseconds /= frequency.QuadPart;
        deltaMicroseconds *= 1000000.0;
        nowMicroSeconds.QuadPart = 
                baseMicroSeconds.QuadPart + deltaMicroseconds;
        
        /* If the frequency too fast, we need to check for wrap around.
         * 2**32 seconds is 136 years, so if HighPart == 0 we don't need to
         * waste a system call on GetSystemTimeAsFileTime.
         */
        if (frequency.HighPart != 0) {
                LARGE_INTEGER nowLowResMicroSeconds;
                FILETIME ft;
                
                /* Use low res timer to detect performance counter wrap-around. */
                GetSystemTimeAsFileTime (&ft);
                nowLowResMicroSeconds.LowPart = ft.dwLowDateTime;
                nowLowResMicroSeconds.HighPart = ft.dwHighDateTime;
                nowLowResMicroSeconds.QuadPart -= EPOCHFILETIME;
                nowLowResMicroSeconds.QuadPart /= 10;
                
                /* If deltaMicroseconds deviates by more than a second from the low
                 * resolution timer, assume the high performance counter has wrapped.
                 * One second is a safe margin b/c QueryPerformanceFrequency must fit
                 * in a 64-bit integer. Therefore any wrap must exceed one second.
                 */
                if (nowMicroSeconds.QuadPart + 1000000 <  nowLowResMicroSeconds.QuadPart) {
                        baseCounter = nowCounter;
                        baseMicroSeconds = nowLowResMicroSeconds;
                        nowMicroSeconds = nowLowResMicroSeconds;
                }
                
                /* The above wrap-around detection destroys high resolution timing.
                 * However, if one needs high resolution timing, then one is querying
                 * gettimeofday quite often. Therefore, rebase the clock before any
                 * wrap around troubles happen. We don't do this too often as it 
                 * introduces clock drift.
                 */
                if ((deltaCounter.HighPart & 0xffff0000UL) != 0) {
                        baseCounter = nowCounter;
                        baseMicroSeconds = nowMicroSeconds;
                }
        }
        
        tv->tv_sec = (long)(nowMicroSeconds.QuadPart / 1000000);
        tv->tv_usec = (long)(nowMicroSeconds.QuadPart % 1000000);
        
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

static void catcher(__attribute__ ((unused)) int signo) {
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
        OSVERSIONINFOEX osv;
        const char* os = "??";

#ifndef _WIN64
        /* Call GetNativeSystemInfo if supported or GetSystemInfo otherwise. */
        SYSTEM_INFO si;
        void (WINAPI *pGNSI)(LPSYSTEM_INFO);
        pGNSI = (void(WINAPI *)(LPSYSTEM_INFO))
                GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                               "GetNativeSystemInfo");
        if (NULL != pGNSI)
          pGNSI(&si);
        else
          GetSystemInfo(&si);
#endif

        osv.dwOSVersionInfoSize = sizeof (osv);
        /* Try to get extended information in order to be able to match the O.S. more
           precisely using osv.wProductType */
        if (! GetVersionEx ((OSVERSIONINFO *)(void*) &osv)) {
          ZeroMemory(&osv, sizeof(OSVERSIONINFOEX));
          osv.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
          GetVersionEx((OSVERSIONINFO *)(void*) &osv);
        }
        switch (osv.dwPlatformId) {
        case VER_PLATFORM_WIN32_NT:
#ifdef _WIN64
                if (osv.dwMinorVersion == 0) {
                  if (osv.dwMajorVersion <= 6) {
                    if (osv.wProductType == VER_NT_WORKSTATION)
                                                          os = "Vista_64";
                    else
                                                          os = "2008_64";
                  } else                                  os = "NTx_64";
                } else if (osv.dwMinorVersion <= 2)       os = "XP_64";
                else                                      os = "NTx_64";
#else
                if (osv.dwMinorVersion == 0) {
                        if (osv.dwMajorVersion <= 4)      os = "NT";
                        else if (osv.dwMajorVersion <= 5) os = "2000";
                        else {
                          if (osv.wProductType == VER_NT_WORKSTATION) {
                            if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
                                                          os = "Vista_WOW64";
                            else
                                                          os = "Vista";
                          } else {
                            if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
                                                          os = "2008";
                            else
                                                          os = "2008_WOW64";
                          }
                        }
                } else if (osv.dwMinorVersion <= 1)       os = "XP";
                else if (osv.dwMinorVersion <= 2) {
                  if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
                                                          os = "XP_WOW64";
                  else
                                                          os = "2003";
                } else                                    os = "NTx";
#endif
                break;
        case VER_PLATFORM_WIN32_WINDOWS:
                if (osv.dwMinorVersion == 0)              os = "95";
                else if (osv.dwMinorVersion < 90)         os = "98";
                else if (osv.dwMinorVersion == 90)        os = "Me";
                else                                      os = "9X";
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
        sprintf (buf->release, "%d", __MINGW64_VERSION_MINOR);
        sprintf (buf->version, "%d", __MINGW64_VERSION_MAJOR);
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

int kill (pid_t pid, int sig) {
        HANDLE h = (HANDLE)pid;
        unless (TerminateProcess (h, SIGNALLED_BIT | sig)) {
                errno = ECHILD;
                return -1;
        }
        return 0;
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

pid_t waitpid (pid_t pid, int *status, int options) {
        HANDLE h;
        DWORD delay;

        /* pid <= 0 is handled in stub-mingw.sml */
        h = (HANDLE)pid;

        delay = ((options & WNOHANG) != 0) ? 0 : INFINITE;

        switch (WaitForSingleObject (h, delay)) {
        case WAIT_OBJECT_0: /* process has exited */
                break;
        case WAIT_TIMEOUT:  /* process has not exited */
                return 0;
        default:            /* some sort of error */
                errno = ECHILD;
                return -1;
        }

        unless (GetExitCodeProcess (h, (DWORD*)status)) {
                errno = ECHILD;
                return -1;
        }

        return pid;
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

/* This table was constructed with help of 
 *   http://msdn.microsoft.com/en-us/library/ms740668(VS.85).aspx#winsock.wsaenotsock_2
 *   man errno(3)
 */
void MLton_fixSocketErrno (void) {
        int status = WSAGetLastError ();
        
        switch (status) {
        case 0:                  errno = 0;               break;
        case WSAEINTR:           errno = EINTR;           break;
        case WSAEBADF:           errno = EBADF;           break;
        case WSAEACCES:          errno = EACCES;          break;
        case WSAEFAULT:          errno = EFAULT;          break;
        case WSAEINVAL:          errno = EINVAL;          break;
        case WSAEMFILE:          errno = EMFILE;          break;
        case WSAEWOULDBLOCK:     errno = EWOULDBLOCK;     break;
        case WSAEINPROGRESS:     errno = EINPROGRESS;     break;
        case WSAEALREADY:        errno = EALREADY;        break;
        case WSAENOTSOCK:        errno = ENOTSOCK;        break;
        case WSAEDESTADDRREQ:    errno = EDESTADDRREQ;    break;
        case WSAEMSGSIZE:        errno = EMSGSIZE;        break;
        case WSAEPROTOTYPE:      errno = EPROTOTYPE;      break;
        case WSAENOPROTOOPT:     errno = ENOPROTOOPT;     break;
        case WSAEPROTONOSUPPORT: errno = EPROTONOSUPPORT; break;
        case WSAESOCKTNOSUPPORT: errno = ESOCKTNOSUPPORT; break;
        case WSAEOPNOTSUPP:      errno = EOPNOTSUPP;      break;
        case WSAEPFNOSUPPORT:    errno = EPFNOSUPPORT;    break;
        case WSAEAFNOSUPPORT:    errno = EAFNOSUPPORT;    break;
        case WSAEADDRINUSE:      errno = EADDRINUSE;      break;
        case WSAEADDRNOTAVAIL:   errno = EADDRNOTAVAIL;   break;
        case WSAENETDOWN:        errno = ENETDOWN;        break;
        case WSAENETUNREACH:     errno = ENETUNREACH;     break;
        case WSAENETRESET:       errno = ENETRESET;       break;
        case WSAECONNABORTED:    errno = ECONNABORTED;    break;
        case WSAECONNRESET:      errno = ECONNRESET;      break;
        case WSAENOBUFS:         errno = ENOBUFS;         break;
        case WSAEISCONN:         errno = EISCONN;         break;
        case WSAENOTCONN:        errno = ENOTCONN;        break;
        case WSAESHUTDOWN:       errno = ESHUTDOWN;       break;
        case WSAETIMEDOUT:       errno = ETIMEDOUT;       break;
        case WSAECONNREFUSED:    errno = ECONNREFUSED;    break;
        case WSAELOOP:           errno = ELOOP;           break;
        case WSAENAMETOOLONG:    errno = ENAMETOOLONG;    break;
        case WSAEHOSTDOWN:       errno = EHOSTDOWN;       break;
        case WSAEHOSTUNREACH:    errno = EHOSTUNREACH;    break;
        case WSAENOTEMPTY:       errno = ENOTEMPTY;       break;
        case WSAEDQUOT:          errno = EDQUOT;          break;
        case WSAESTALE:          errno = ESTALE;          break;
        case WSAEREMOTE:         errno = EREMOTE;         break;
        /* These codes appear to have a matching name, but the manual
         * descriptions of what the error codes mean seem to differ
         */
        case WSAEUSERS:          errno = EUSERS;          break;
        case WSAECANCELLED:      errno = ECANCELED;       break;
        case WSA_E_CANCELLED:    errno = ECANCELED;       break;
        /* These codes have no matching code in the errno(3) man page. */
        case WSAEPROCLIM:        errno = EBUSY;           break;
        case WSAETOOMANYREFS:    errno = ENOMEM;          break;
        case WSAEDISCON:         errno = ESHUTDOWN;       break;
        case WSA_E_NO_MORE:
        case WSAENOMORE:
        case WSASYSCALLFAILURE:  errno = EIO;             break;
        /* These codes are returned from the OS and subject to chage */
        // WSA_INVALID_HANDLE
        // WSA_NOT_ENOUGH_MEMORY
        // WSA_INVALID_PARAMETER
        // WSA_OPERATION_ABORTED
        // WSA_IO_INCOMPLETE
        // WSA_IO_PENDING
        /* These codes mean some sort of windows specific fatal error */
        case WSASYSNOTREADY: 
        case WSAVERNOTSUPPORTED:
        case WSANOTINITIALISED:
        case WSAEINVALIDPROCTABLE:
        case WSAEINVALIDPROVIDER:
        case WSAEPROVIDERFAILEDINIT:
        case WSASERVICE_NOT_FOUND:
        case WSATYPE_NOT_FOUND:
                                 die("Problem loading winsock");
        case WSAEREFUSED:
        case WSAHOST_NOT_FOUND:
        case WSATRY_AGAIN:
        case WSANO_RECOVERY:
        case WSANO_DATA:
                                 die("Strange winsock specific status code");
        default:
                                 die("Unknown winsock status code");
        }
}

static const char *MLton_strerrorExtension(int code) {
        switch (code) {
        case EINTR:           return "Interrupted function call";
        case EBADF:           return "Bad file descriptor";
        case EACCES:          return "Permission denied";
        case EFAULT:          return "Bad address";
        case EINVAL:          return "Invalid argument";
        case EMFILE:          return "Too many open files";
        case EAGAIN:          return "Resource temporarily unavailable";
        case EINPROGRESS:     return "Operation in progress";
        case EALREADY:        return "Connection already in progress";
        case ENOTSOCK:        return "Not a socket";
        case EDESTADDRREQ:    return "Destination address required";
        case EMSGSIZE:        return "Message too long";
        case EPROTOTYPE:      return "Protocol wrong type for socket";
        case ENOPROTOOPT:     return "Protocol not available";
        case EPROTONOSUPPORT: return "Protocol not supported";
        case ESOCKTNOSUPPORT: return "Socket type not supported";
        case EOPNOTSUPP:      return "Operation not supported on socket";
        case EPFNOSUPPORT:    return "Protocol family not supported";
        case EAFNOSUPPORT:    return "Address family not supported";
        case EADDRINUSE:      return "Address already in use";
        case EADDRNOTAVAIL:   return "Address not available";
        case ENETDOWN:        return "Network is down";
        case ENETUNREACH:     return "Network unreachable";
        case ENETRESET:       return "Connection aborted by network";
        case ECONNABORTED:    return "Connection aborted";
        case ECONNRESET:      return "Connection reset";
        case ENOBUFS:         return "No buffer space available";
        case EISCONN:         return "Socket is connected";
        case ENOTCONN:        return "The socket is not connected";
        case ESHUTDOWN:       return "Cannot send after transport endpoint shutdown";
        case ETIMEDOUT:       return "Connection timed out";
        case ECONNREFUSED:    return "Connection refused";
        case ELOOP:           return "Too many levels of symbolic links";
        case ENAMETOOLONG:    return "Filename too long";
        case EHOSTDOWN:       return "Host is down";
        case EHOSTUNREACH:    return "Host is unreachable";
        case ENOTEMPTY:       return "Directory not empty";
        case EDQUOT:          return "Disk quota exceeded";
        case ESTALE:          return "Stale file handle";
        case EREMOTE:         return "Object is remote";
        case EUSERS:          return "Too many users";
        case ECANCELED:       return "Operation canceled";
        default:              return "Unknown error";
        }
}

/* MinGW strerror works for all system-defined errno values.
 * However, platform/mingw.h adds some missing POSIX networking error codes.
 * It defines these codes as their closest-equivalent winsock error code.
 * To report network errors, MLton_fixSocketErrno maps winsock errors to 
 * their closest POSIX errno value.
 * 
 * This function must handle the winsock errno values we have added.
 * FormatMessage doesn't return the POSIX string for errors, and it uses
 * the current locale's language. The MinGW strerror is always English.
 * 
 * Thus, we just make a big English table to augment strerror.
 * The descriptions are taken from man errno(3).
 */
char *MLton_strerror(int code) {
        static char buffer[80];
        
#undef strerror
        if (code < sys_nerr) return strerror(code);
#define strerror MLton_strerror

        strcpy(buffer, MLton_strerrorExtension(code));
        return buffer;
}

int MLton_recv(int s, void *buf, int len, int flags) {
        int ret, status = 0;
        
        if (flags & MSG_DONTWAIT) MinGW_setNonBlock(s);
        ret = recv(s, buf, len, flags & ~MSG_DONTWAIT);
        
        /* We need to preserve the error status across non-blocking call */
        if (ret == -1) status = WSAGetLastError();
        if (flags & MSG_DONTWAIT) MinGW_clearNonBlock(s);
        if (ret == -1) WSASetLastError(status);
        
        return ret;
}

int MLton_recvfrom(int s, void *buf, int len, int flags, void *from,
                   socklen_t *fromlen) {
        int ret, status = 0;
        
        if (flags & MSG_DONTWAIT) MinGW_setNonBlock(s);
        ret = recvfrom(s, buf, len, flags & ~MSG_DONTWAIT, from, fromlen);
        
        /* We need to preserve the error status across non-blocking call */
        if (ret == -1) status = WSAGetLastError();
        if (flags & MSG_DONTWAIT) MinGW_clearNonBlock(s);
        if (ret == -1) WSASetLastError(status);
        
        return ret;
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

void MinGW_setNonBlock(C_Fd_t fd) {
        unsigned long yes = 1;
        ioctlsocket(fd, FIONBIO, &yes);
}

void MinGW_clearNonBlock(C_Fd_t fd) {
        unsigned long no = 0;
        ioctlsocket(fd, FIONBIO, &no);
}
