/* Many of the functions used in mingw.c are Win2000+ */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0500
#endif

#include <inttypes.h>
#include <stdint.h>

#include <unistd.h>
#include <winsock2.h>
#include <windows.h>

#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <utime.h>
#include <time.h>

#include <io.h>
#include <lm.h>
#include <process.h>
//#include <psapi.h>
#include <ws2tcpip.h>
#include <psapi.h>

#undef max

// As of 20080807, MinGW has a broken fesetround. Use the runtime's.
#define HAS_FEROUND FALSE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_REMAP TRUE
#define HAS_SIGALTSTACK FALSE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN TRUE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "mingw"

/** MinGW is a moving target. MLton seems to break every release.
 *  Often new releases add new emulation for POSIX functions.
 *  We need to provide wrappers for some POSIX functions, but
 *  these wrappers end up conflicting with MinGW if they're added.
 *  Therefore, all wrapper stuff we add will be #define'd to use
 *  a MLton_prefix, thus avoiding collision with MinGW versions,
 *  current, old, or future. Macros themselves can conflict, so
 *  we need to #undef any old value, and in the case of constants,
 *  wrap the define in an ifndef. It's ugly and verbose, but safe.
 */

#define MLTON_WRAPPER

typedef unsigned short MLton_gid_t;
typedef unsigned short MLton_uid_t;
typedef long MLton_suseconds_t; // type of timeval.tv_usec in sys/time.h
typedef short MLton_nlink_t; // type of st_nlink in sys/stat.h
typedef unsigned int MLton_nfds_t; // we have a fake poll() with this many fds

#undef gid_t
#undef uid_t
#undef suseconds_t
#undef nlink_t
#undef nfds_t

#define gid_t MLton_gid_t
#define uid_t MLton_uid_t
#define suseconds_t MLton_suseconds_t
#define nlink_t MLton_nlink_t
#define nfds_t MLton_nfds_t

MLTON_WRAPPER int MLton_getpagesize (void);
MLTON_WRAPPER int MLton_mkstemp (char *template);

#undef getpagesize
#undef mkstemp

#define getpagesize MLton_getpagesize
#define mkstemp MLton_mkstemp

#ifndef POLLIN
#define POLLIN 1
#endif

#ifndef POLLPRI
#define POLLPRI 2
#endif

#ifndef POLLOUT
#define POLLOUT 4
#endif

#ifndef _PC_CHOWN_RESTRICTED
#define _PC_CHOWN_RESTRICTED 6
#endif

#ifndef _PC_LINK_MAX
#define _PC_LINK_MAX 0
#endif

#ifndef _PC_MAX_CANON
#define _PC_MAX_CANON 1
#endif

#ifndef _PC_MAX_INPUT
#define _PC_MAX_INPUT 2
#endif

#ifndef _PC_NAME_MAX
#define _PC_NAME_MAX 3
#endif

#ifndef _PC_NO_TRUNC
#define _PC_NO_TRUNC 7
#endif

#ifndef _PC_PATH_MAX
#define _PC_PATH_MAX 4
#endif

#ifndef _PC_PIPE_BUF
#define _PC_PIPE_BUF 5
#endif

#ifndef _PC_VDISABLE
#define _PC_VDISABLE 8
#endif

#ifndef F_DUPFD
#define F_DUPFD 0
#endif

#ifndef F_GETFD
#define F_GETFD 1
#endif

#ifndef F_SETFD
#define F_SETFD 2
#endif

#ifndef F_GETFL
#define F_GETFL 3
#endif

#ifndef F_SETFL
#define F_SETFL 4
#endif

#ifndef F_GETOWN
#define F_GETOWN 5
#endif

#ifndef F_SETOWN
#define F_SETOWN 6
#endif

#ifndef F_GETLK
#define F_GETLK 7
#endif

#ifndef F_SETLK
#define F_SETLK 8
#endif

#ifndef F_RDLCK
#define F_RDLCK 1
#endif

#ifndef F_WRLCK
#define F_WRLCK 2
#endif

#ifndef F_UNLCK
#define F_UNLCK 3
#endif

#ifndef F_SETLKW
#define F_SETLKW 9
#endif

#ifndef FD_CLOEXEC
#define FD_CLOEXEC 1
#endif

#ifndef SHUT_RD
#define SHUT_RD SD_RECEIVE
#endif

#ifndef SHUT_WR
#define SHUT_WR SD_SEND
#endif

#ifndef SHUT_RDWR
#define SHUT_RDWR SD_BOTH
#endif

/* ------------------------------------------------- */
/*                       Date                        */
/* ------------------------------------------------- */

/* MinGW provides gettimeofday in -lmingwex, which we don't link.
 * In order to avoid a name conflict, we use a different name.
 */
struct MLton_timezone {
      int unused;
};
#undef timezone
#define timezone MLton_timezone

MLTON_WRAPPER int MLton_gettimeofday (struct timeval *tv, struct timezone *tz);
#undef gettimeofday
#define gettimeofday MLton_gettimeofday

/* ------------------------------------------------- */
/*                   MLton.Itimer                    */
/* ------------------------------------------------- */

#ifndef ITIMER_REAL
#define ITIMER_REAL    0                /*generates sigalrm */
#endif

#ifndef ITIMER_VIRTUAL
#define ITIMER_VIRTUAL 1                /*generates sigvtalrm */
#endif

#ifndef ITIMER_VIRT
#define ITIMER_VIRT    1                /*generates sigvtalrm */
#endif

#ifndef ITIMER_PROF
#define ITIMER_PROF    2                /*generates sigprof */
#endif

struct MLton_itimerval {
        struct timeval it_interval;
        struct timeval it_value;
};
#undef itimerval
#define itimerval MLton_itimerval

MLTON_WRAPPER int MLton_setitimer (int which,
                              const struct itimerval *value,
                              struct itimerval *ovalue);
#undef setitimer
#define setitimer MLton_setitimer

/* ------------------------------------------------- */
/*                   MLton.Rlimit                    */
/* ------------------------------------------------- */

#ifndef RLIMIT_CPU
#define RLIMIT_CPU      0               /* CPU time in seconds */
#endif

#ifndef RLIMIT_FSIZE
#define RLIMIT_FSIZE    1               /* Maximum filesize */
#endif

#ifndef RLIMIT_DATA
#define RLIMIT_DATA     2               /* max data size */
#endif

#ifndef RLIMIT_STACK
#define RLIMIT_STACK    3               /* max stack size */
#endif

#ifndef RLIMIT_CORE
#define RLIMIT_CORE     4               /* max core file size */
#endif

#ifndef RLIMIT_NOFILE
#define RLIMIT_NOFILE   5               /* max number of open files */
#endif

#ifndef RLIMIT_OFILE
#define RLIMIT_OFILE    RLIMIT_NOFILE   /* BSD name */
#endif

#ifndef RLIMIT_AS
#define RLIMIT_AS       6               /* address space (virt. memory) limit */
#endif

#define RLIMIT_NLIMITS  7               /* upper bound of RLIMIT_* defines */
#define RLIM_NLIMITS    RLIMIT_NLIMITS

#ifndef RLIM_INFINITY
#define RLIM_INFINITY   (0xffffffffUL)
#endif

#ifndef RLIM_SAVED_MAX
#define RLIM_SAVED_MAX  RLIM_INFINITY
#endif

#ifndef RLIM_SAVED_CUR
#define RLIM_SAVED_CUR  RLIM_INFINITY
#endif

typedef unsigned long MLton_rlim_t;
#undef rlim_t
#define rlim_t MLton_rlim_t

struct MLton_rlimit {
        rlim_t  rlim_cur;
        rlim_t  rlim_max;
};
#undef rlimit
#define rlimit MLton_rlimit

MLTON_WRAPPER int MLton_getrlimit (int resource, struct rlimit *rlim);
MLTON_WRAPPER int MLton_setrlimit (int resource, const struct rlimit *rlim);
#undef getrlimit
#undef setrlimit
#define getrlimit MLton_getrlimit
#define setrlimit MLton_setrlimit

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

#ifndef RUSAGE_SELF
#define RUSAGE_SELF 0               /* calling process */
#endif

#ifndef RUSAGE_CHILDREN
#define RUSAGE_CHILDREN -1              /* terminated child processes */
#endif

struct MLton_rusage {
        struct timeval ru_utime;
        struct timeval ru_stime;
};
#undef rusage
#define rusage MLton_rusage

MLTON_WRAPPER int MLton_getrusage (int who, struct rusage *usage);
#undef getrusage
#define getrusage MLton_getrusage

/* ------------------------------------------------- */
/*                       OS.IO                       */
/* ------------------------------------------------- */

struct MLton_pollfd {
        short events;
        int fd;
        short revents;
};
#undef pollfd
#define pollfd MLton_pollfd

MLTON_WRAPPER int MLton_poll (struct pollfd *ufds, nfds_t nfds, int timeout);
#undef poll
#define poll MLton_poll

/* ------------------------------------------------- */
/*                    Posix.Error                    */
/* ------------------------------------------------- */

/* We cannot yet replace strerror at the time util.c is built */
#ifndef MLTON_UTIL
MLTON_WRAPPER char *MLton_strerror(int code);
#undef strerror
#define strerror MLton_strerror
#endif

/* If MinGW doesn't (currently) define an error status we need, but winsock
 * does, then default to using the winsock status. They will not conflict.
 */

#ifndef EINTR
#define EINTR WSAEINTR
#endif

#ifndef EBADF
#define EBADF WSAEBADF
#endif

#ifndef EACCES
#define EACCES WSAEACCES
#endif

#ifndef EFAULT
#define EFAULT WSAEFAULT
#endif

#ifndef EINVAL
#define EINVAL WSAEINVAL
#endif

#ifndef EMFILE
#define EMFILE WSAEMFILE
#endif

#ifndef EAGAIN
#define EAGAIN WSAEWOULDBLOCK
#endif

#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
#endif

#ifndef EINPROGRESS
#define EINPROGRESS WSAEINPROGRESS
#endif

#ifndef EALREADY
#define EALREADY WSAEALREADY
#endif

#ifndef ENOTSOCK
#define ENOTSOCK WSAENOTSOCK
#endif

#ifndef EDESTADDRREQ
#define EDESTADDRREQ WSAEDESTADDRREQ
#endif

#ifndef EMSGSIZE
#define EMSGSIZE WSAEMSGSIZE
#endif

#ifndef EPROTOTYPE
#define EPROTOTYPE WSAEPROTOTYPE
#endif

#ifndef ENOPROTOOPT
#define ENOPROTOOPT WSAENOPROTOOPT
#endif

#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT WSAEPROTONOSUPPORT
#endif

#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT WSAESOCKTNOSUPPORT
#endif

#ifndef EOPNOTSUPP
#define EOPNOTSUPP WSAEOPNOTSUPP
#endif

#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT WSAEPFNOSUPPORT
#endif

#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT WSAEAFNOSUPPORT
#endif

#ifndef EADDRINUSE
#define EADDRINUSE WSAEADDRINUSE
#endif

#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL WSAEADDRNOTAVAIL
#endif

#ifndef ENETDOWN
#define ENETDOWN WSAENETDOWN
#endif

#ifndef ENETUNREACH
#define ENETUNREACH WSAENETUNREACH
#endif

#ifndef ENETRESET
#define ENETRESET WSAENETRESET
#endif

#ifndef ECONNABORTED
#define ECONNABORTED WSAECONNABORTED
#endif

#ifndef ECONNRESET
#define ECONNRESET WSAECONNRESET
#endif

#ifndef ENOBUFS
#define ENOBUFS WSAENOBUFS
#endif

#ifndef EISCONN
#define EISCONN WSAEISCONN
#endif

#ifndef ENOTCONN
#define ENOTCONN WSAENOTCONN
#endif

#ifndef ESHUTDOWN
#define ESHUTDOWN WSAESHUTDOWN
#endif

#ifndef ETIMEDOUT
#define ETIMEDOUT WSAETIMEDOUT
#endif

#ifndef ECONNREFUSED
#define ECONNREFUSED WSAECONNREFUSED
#endif

#ifndef ELOOP
#define ELOOP WSAELOOP
#endif

#ifndef ENAMETOOLONG
#define ENAMETOOLONG WSAENAMETOOLONG
#endif

#ifndef EHOSTDOWN
#define EHOSTDOWN WSAEHOSTDOWN
#endif

#ifndef EHOSTUNREACH
#define EHOSTUNREACH WSAEHOSTUNREACH
#endif

#ifndef ENOTEMPTY
#define ENOTEMPTY WSAENOTEMPTY
#endif

#ifndef EDQUOT
#define EDQUOT WSAEDQUOT
#endif

#ifndef ESTALE
#define ESTALE WSAESTALE
#endif

#ifndef ERMOTE
#define EREMOTE WSAEREMOTE
#endif

/* Questionable fall backs: */

#ifndef EUSERS
#define EUSERS WSAEUSERS
#endif

#ifndef ECANCELED
#define ECANCELED WSAECANCELLED
#endif

#ifndef EBADMSG
#define EBADMSG 77
#endif

/* ------------------------------------------------- */
/*                   Posix.FileSys                   */
/* ------------------------------------------------- */

#ifndef S_IRGRP
#define S_IRGRP 0000040
#endif

#ifndef S_IROTH
#define S_IROTH 0000004
#endif

#ifndef S_IRWXG
#define S_IRWXG 0000070
#endif

#ifndef S_IRWXO
#define S_IRWXO 0000007
#endif

#ifndef S_ISGID
#define S_ISGID 0002000
#endif

#ifndef S_ISUID
#define S_ISUID 0004000
#endif

#ifndef S_IWGRP
#define S_IWGRP 0000020
#endif

#ifndef S_IWOTH
#define S_IWOTH 0000002
#endif

#ifndef S_IXGRP
#define S_IXGRP 0000010
#endif

#ifndef S_IXOTH
#define S_IXOTH 0000001
#endif

// Do not exist in a windows filesystem
#ifndef S_IFLNK
#define S_IFLNK 0
#endif

#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif

#ifndef S_ISVTX
#define S_ISVTX 0
#endif

#ifndef O_NOCTTY
#define O_NOCTTY 0x8000
#endif

#ifndef O_NONBLOCK
#define O_NONBLOCK 0x4000
#endif

// Synchronized writes? Safety of any kind? ... and windows?! hell no!
#ifndef O_SYNC
#define O_SYNC 0
#endif

// Use m to silence unused warnings
#undef S_ISLNK
#undef S_ISSOCK
#define S_ISLNK(m) (m?FALSE:FALSE)
#define S_ISSOCK(m) (m?FALSE:FALSE)

#ifndef O_ACCMODE
#define O_ACCMODE O_RDONLY|O_WRONLY|O_RDWR
#endif

MLTON_WRAPPER int MLton_chown (const char *path, uid_t owner, gid_t group);
MLTON_WRAPPER int MLton_fchmod (int filedes, mode_t mode);
MLTON_WRAPPER int MLton_fchdir (int filedes);
MLTON_WRAPPER int MLton_fchown (int fd, uid_t owner, gid_t group);
MLTON_WRAPPER long MLton_fpathconf (int filedes, int name);
MLTON_WRAPPER int MLton_link (const char *oldpath, const char *newpath);
MLTON_WRAPPER int MLton_lstat (const char *file_name, struct stat *buf);
MLTON_WRAPPER int MLton_mkfifo (const char *pathname, mode_t mode);
MLTON_WRAPPER long MLton_pathconf (const char *path, int name);
MLTON_WRAPPER int MLton_readlink (const char *path, char *buf, size_t bufsiz);
MLTON_WRAPPER int MLton_symlink (const char *oldpath, const char *newpath);
MLTON_WRAPPER int MLton_truncate (const char *path, off_t len);

#undef chown
#undef fchmod
#undef fchdir
#undef fchown
#undef fpathconf
#undef link
#undef lstat
#undef mkfifo
#undef pathconf
#undef readlink
#undef symlink
#undef truncate

#define chown MLton_chown
#define fchmod MLton_fchmod
#define fchdir MLton_fchdir
#define fchown MLton_fchown
#define fpathconf MLton_fpathconf
#define link MLton_link
#define lstat MLton_lstat
#define mkfifo MLton_mkfifo
#define pathconf MLton_pathconf
#define readlink MLton_readlink
#define symlink MLton_symlink
#define truncate MLton_truncate

#undef mkdir
#define mkdir(f, m) mkdir(f); chmod(f, m)

/* ------------------------------------------------- */
/*                     Posix.IO                      */
/* ------------------------------------------------- */

struct MLton_flock {
        off_t l_len;
        pid_t l_pid;
        off_t l_start;
        short l_type;
        short l_whence;
};
#undef flock
#define flock MLton_flock

MLTON_WRAPPER int MLton_fcntl (int fd, int cmd, ...);
MLTON_WRAPPER int MLton_fsync (int fd);
MLTON_WRAPPER int MLton_pipe (int filedes[2]);

#undef fcntl
#undef fsync
#undef pipe

#define fcntl MLton_fcntl
#define fsync MLton_fsync
#define pipe MLton_pipe

/* ------------------------------------------------- */
/*                   Posix.ProcEnv                   */
/* ------------------------------------------------- */

#ifndef _SC_ARG_MAX
#define _SC_ARG_MAX 0
#endif

#ifndef _SC_CHILD_MAX
#define _SC_CHILD_MAX 1
#endif

#ifndef _SC_CLK_TCK
#define _SC_CLK_TCK 2
#endif

#ifndef _SC_JOB_CONTROL
#define _SC_JOB_CONTROL 5
#endif

#ifndef _SC_NGROUPS_MAX
#define _SC_NGROUPS_MAX 3
#endif

#ifndef _SC_OPEN_MAX
#define _SC_OPEN_MAX 4
#endif

#ifndef _SC_SAVED_IDS
#define _SC_SAVED_IDS 6
#endif

#ifndef _SC_VERSION
#define _SC_VERSION 7
#endif

struct MLton_tms {
        int tms_utime;
        int tms_stime;
        int tms_cutime;
        int tms_cstime;
};

struct MLton_utsname {
        char machine[20];
        char nodename[256];
        char release[20];
        char sysname[30];
        char version[20];
};

#undef tms
#undef utsname

#define tms MLton_tms
#define utsname MLton_utsname

MLTON_WRAPPER char *MLton_getlogin (void);
MLTON_WRAPPER char *MLton_ctermid (char *s);
MLTON_WRAPPER gid_t MLton_getegid (void);
MLTON_WRAPPER uid_t MLton_geteuid (void);
MLTON_WRAPPER gid_t MLton_getgid (void);
MLTON_WRAPPER int MLton_getgroups (int size, gid_t list[]);
MLTON_WRAPPER pid_t MLton_getpgid(pid_t pid);
MLTON_WRAPPER pid_t MLton_getpgrp(void);
MLTON_WRAPPER pid_t MLton_getppid (void);
MLTON_WRAPPER uid_t MLton_getuid (void);
MLTON_WRAPPER int MLton_setenv (const char *name, const char *value, int overwrite);
MLTON_WRAPPER int MLton_setgid (gid_t gid);
MLTON_WRAPPER int MLton_setgroups (size_t size, const gid_t *list);
MLTON_WRAPPER int MLton_setpgid (pid_t pid, pid_t pgid);
MLTON_WRAPPER pid_t MLton_setsid (void);
MLTON_WRAPPER int MLton_setuid (uid_t uid);
MLTON_WRAPPER long MLton_sysconf (int name);
MLTON_WRAPPER clock_t MLton_times (struct tms *buf);
MLTON_WRAPPER char *MLton_ttyname (int desc);
MLTON_WRAPPER int MLton_uname (struct utsname *buf);

#undef getlogin
#undef ctermid
#undef getegid
#undef geteuid
#undef getgid
#undef getgroups
#undef getpgid
#undef getpgrp
#undef getppid
#undef getuid
#undef setenv
#undef setgid
#undef setgroups
#undef setpgid
#undef setsid
#undef setuid
#undef sysconf
#undef times
#undef ttyname
#undef uname

#define getlogin MLton_getlogin
#define ctermid MLton_ctermid
#define getegid MLton_getegid
#define geteuid MLton_geteuid
#define getgid MLton_getgid
#define getgroups MLton_getgroups
#define getpgid MLton_getpgid
#define getpgrp MLton_getpgrp
#define getppid MLton_getppid
#define getuid MLton_getuid
#define setenv MLton_setenv
#define setgid MLton_setgid
#define setgroups MLton_setgroups
#define setpgid MLton_setpgid
#define setsid MLton_setsid
#define setuid MLton_setuid
#define sysconf MLton_sysconf
#define times MLton_times
#define ttyname MLton_ttyname
#define uname MLton_uname


/* ------------------------------------------------- */
/*                   Posix.Process                   */
/* ------------------------------------------------- */

#define EXECVE(path, args, env)         \
        execve (path, (const char* const*)args, (const char* const*)env)
#define EXECVP(file, args)  execvp (file, (const char* const*) args)
#define SPAWN_MODE _P_NOWAIT

/* Windows exit status comes from:
 *  1. ExitProcess (used by return from main and exit)
 *  2. TerminateProcess (used by a remote process to 'kill')
 *
 * Windows does NOT differentiate between these two cases.
 * The waitpid API expects us to be able to tell the difference,
 * so we will emulate this difference by setting high 31st bit 
 * whenever we 'kill' a process.
 */

#ifndef WNOHANG
#define WNOHANG 1
#endif

#ifndef WUNTRACED
#define WUNTRACED 2
#endif

#define SIGNALLED_BIT   0x80000000UL

#ifndef WIFEXITED
#define WIFEXITED(w)    (((w) & SIGNALLED_BIT) == 0)
#endif

#ifndef WIFSIGNALED
#define WIFSIGNALED(w)  (((w) & SIGNALLED_BIT) != 0)
#endif

#ifndef WIFSTOPPED
#define WIFSTOPPED(w)   0
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(w)  ((w) & 0xff)
#endif

#ifndef WTERMSIG
#define WTERMSIG(w)     ((w) & 0xff)
#endif

#ifndef WSTOPSIG
#define WSTOPSIG        WEXITSTATUS
#endif

/* Sometimes defined by mingw */
#if !defined(TIMESPEC_DEFINED) && !defined(_TIMESPEC_DEFINED)
struct timespec {
  time_t tv_sec;
  long tv_nsec;
};
#endif

MLTON_WRAPPER int MLton_alarm(int);
MLTON_WRAPPER int MLton_fork(void); /* mingw demands this return int */
MLTON_WRAPPER int MLton_kill (pid_t pid, int sig);
MLTON_WRAPPER int MLton_pause (void);
MLTON_WRAPPER int MLton_nanosleep (const struct timespec *req, struct timespec *rem);
MLTON_WRAPPER unsigned int MLton_sleep (unsigned int seconds);
MLTON_WRAPPER pid_t MLton_wait (int *status);
MLTON_WRAPPER pid_t MLton_waitpid (pid_t pid, int *status, int options);

#undef alarm
#undef fork
#undef kill
#undef pause
#undef nanosleep
#undef sleep
#undef wait
#undef waitpid

#define alarm MLton_alarm
#define fork MLton_fork
#define kill MLton_kill
#define pause MLton_pause
#define nanosleep MLton_nanosleep
#define sleep MLton_sleep
#define wait MLton_wait
#define waitpid MLton_waitpid

/* ------------------------------------------------- */
/*                   Posix.Signal                    */
/* ------------------------------------------------- */

/* Sometimes mingw defines some of these. Some not. Some always. */

#ifndef SIG_BLOCK
#define SIG_BLOCK 1
#endif

#ifndef SIG_SETMASK
#define SIG_SETMASK 0
#endif

#ifndef SIG_UNBLOCK
#define SIG_UNBLOCK 2
#endif

#ifndef SIGHUP
#define SIGHUP 1
#endif

/* SIGINT = 2 */

#ifndef SIGQUIT
#define SIGQUIT 3
#endif

/* SIGILL  = 4 */
/* SIGTRAP = 5 (unused) */
/* SIGIOT  = 6 (unused) */
/* SIGABRT = 6 (unused) */
/* SIGEMT  = 7 (unused) */
/* SIGFPE  = 8 */

#ifndef SIGKILL
#define SIGKILL 9
#endif

#ifndef SIGBUS
#define SIGBUS 10
#endif

/* SIGSEGV = 11 */
/* SIGSYS = 12 (unused) */

#ifndef SIGPIPE
#define SIGPIPE 13
#endif

#ifndef SIGALRM
#define SIGALRM 14
#endif

/* SIGTERM = 15 */
/* SIGBREAK = 21 */
/* SIGABRT2 = 22 */

/* These signals are fake. They do not exist on windows. */

#ifndef SIGSTOP
#define SIGSTOP 16
#endif

#ifndef SIGTSTP
#define SIGTSTP 18
#endif

#ifndef SIGCHLD
#define SIGCHLD 23
#endif

#ifndef SIGTTIN
#define SIGTTIN 24
#endif

#ifndef SIGTTOU
#define SIGTTOU 25
#endif

#ifndef SIGCONT
#define SIGCONT 26
#endif

#ifndef SIGUSR1
#define SIGUSR1 27
#endif

#ifndef SIGUSR2
#define SIGUSR2 28
#endif

#ifndef SIGVTALRM
#define SIGVTALRM 29    /* virtual time alarm */
#endif

#ifndef SIGPROF
#define SIGPROF 30      /* profiling time alarm */
#endif

/* We have extended the number of signals ... */
#ifdef NSIG
#undef NSIG
#endif
#define NSIG 32

typedef __p_sig_fn_t MLton__sig_func_ptr;
typedef int MLton_sigset_t;

#undef _sig_func_ptr
#undef sigset_t

#define _sig_func_ptr MLton__sig_func_ptr
#define sigset_t MLton_sigset_t

struct MLton_sigaction {
        int             sa_flags;
        sigset_t        sa_mask;
        _sig_func_ptr   sa_handler;
};

#undef sigaction
#define sigaction MLton_sigaction

#ifndef SIGTOMASK
#define SIGTOMASK(sn)   (1 << ((sn)-1))
#endif

MLTON_WRAPPER int MLton_sigaction (int signum,
                              const struct sigaction *act,
                              struct sigaction *oldact);
MLTON_WRAPPER int MLton_sigaddset (sigset_t *set, int signum);
MLTON_WRAPPER int MLton_sigdelset (sigset_t *set, int signum);
MLTON_WRAPPER int MLton_sigemptyset (sigset_t *set);
MLTON_WRAPPER int MLton_sigfillset (sigset_t *set);
MLTON_WRAPPER int MLton_sigismember (const sigset_t *set, int signum);
MLTON_WRAPPER int MLton_sigpending (sigset_t *set);
MLTON_WRAPPER int MLton_sigprocmask (int how, const sigset_t *set, sigset_t *oldset);
MLTON_WRAPPER int MLton_sigsuspend (const sigset_t *mask);

#undef sigaction
#undef sigaddset
#undef sigdelset
#undef sigemptyset
#undef sigfillset
#undef sigismember
#undef sigpending
#undef sigprocmask
#undef sigsuspend

#define sigaction MLton_sigaction
#define sigaddset MLton_sigaddset
#define sigdelset MLton_sigdelset
#define sigemptyset MLton_sigemptyset
#define sigfillset MLton_sigfillset
#define sigismember MLton_sigismember
#define sigpending MLton_sigpending
#define sigprocmask MLton_sigprocmask
#define sigsuspend MLton_sigsuspend

/* ------------------------------------------------- */
/*                Posix.SysDB.Passwd                 */
/* ------------------------------------------------- */

struct MLton_group {
        gid_t   gr_gid;
        char    **gr_mem;
        char    *gr_name;
        char    *gr_passwd;
};

struct MLton_passwd {
        char    *pw_dir;
        gid_t   pw_gid;
        char    *pw_name;
        char    *pw_shell;
        uid_t   pw_uid;
};

#undef group
#undef passwd
#define group MLton_group
#define passwd MLton_passwd

MLTON_WRAPPER struct group *MLton_getgrgid (gid_t gid);
MLTON_WRAPPER struct group *MLton_getgrnam (const char *name);
MLTON_WRAPPER struct passwd *MLton_getpwnam (const char *name);
MLTON_WRAPPER struct passwd *MLton_getpwuid (uid_t uid);

#undef getgrgid
#undef getgrnam
#undef getpwnam
#undef getpwuid

#define getgrgid MLton_getgrgid
#define getgrnam MLton_getgrnam
#define getpwnam MLton_getpwnam
#define getpwuid MLton_getpwuid

/* ------------------------------------------------- */
/*                     Posix.TTY                     */
/* ------------------------------------------------- */

#ifndef B0
#define B0       0x00000
#endif

#ifndef B50
#define B50      0x00001
#endif

#ifndef B75
#define B75      0x00002
#endif

#ifndef B110
#define B110     0x00003
#endif

#ifndef B134
#define B134     0x00004
#endif

#ifndef B150
#define B150     0x00005
#endif

#ifndef B200
#define B200     0x00006
#endif

#ifndef B300
#define B300     0x00007
#endif

#ifndef B600
#define B600     0x00008
#endif

#ifndef B1200
#define B1200    0x00009
#endif

#ifndef B1800
#define B1800    0x0000a
#endif

#ifndef B2400
#define B2400    0x0000b
#endif

#ifndef B4800
#define B4800    0x0000c
#endif

#ifndef B9600
#define B9600    0x0000d
#endif

#ifndef B19200
#define B19200   0x0000e
#endif

#ifndef B38400
#define B38400   0x0000f
#endif

#ifndef VEOL
#define VEOL            2
#endif

#ifndef VEOL2
#define VEOL2           3
#endif

#ifndef VEOF
#define VEOF            4
#endif

#ifndef VERASE
#define VERASE          5
#endif

#ifndef VINTER
#define VINTR           6
#endif

#ifndef VKILL
#define VKILL           7
#endif

#ifndef VLNEXT
#define VLNEXT          8
#endif

#ifndef VMIN
#define VMIN            9
#endif

#ifndef VQUIT
#define VQUIT           10
#endif

#ifndef VREPRINT
#define VREPRINT        11
#endif

#ifndef VSTART
#define VSTART          12
#endif

#ifndef VSTOP
#define VSTOP           13
#endif

#ifndef VSUSP
#define VSUSP           14
#endif

#ifndef VSWTC
#define VSWTC           15
#endif

#ifndef VTIME
#define VTIME           16
#endif

#ifndef VWERASE
#define VWERASE 17
#endif

#ifndef NCCS
#define NCCS            18
#endif

#ifndef IGNBRK
#define IGNBRK  0x00001
#endif

#ifndef BRKINT
#define BRKINT  0x00002
#endif

#ifndef IGNPAR
#define IGNPAR  0x00004
#endif

#ifndef IMAXBEL
#define IMAXBEL 0x00008
#endif

#ifndef INPCK
#define INPCK   0x00010
#endif

#ifndef ISTRIP
#define ISTRIP  0x00020
#endif

#ifndef INLCR
#define INLCR   0x00040
#endif

#ifndef IGNCR
#define IGNCR   0x00080
#endif

#ifndef ICRNL
#define ICRNL   0x00100
#endif

#ifndef IXON
#define IXON    0x00400
#endif

#ifndef IXOFF
#define IXOFF   0x01000
#endif

#ifndef IUCLC
#define IUCLC   0x04000
#endif

#ifndef IXANY
#define IXANY   0x08000
#endif

#ifndef PARMRK
#define PARMRK  0x10000
#endif

#ifndef OPOST
#define OPOST   0x00001
#endif

#ifndef CSIZE
#define CSIZE    0x00030
#endif

#ifndef CS5
#define CS5      0x00000
#endif

#ifndef CS6
#define CS6      0x00010
#endif

#ifndef CS7
#define CS7      0x00020
#endif

#ifndef CS8
#define CS8      0x00030
#endif

#ifndef CSTOPB
#define CSTOPB   0x00040
#endif

#ifndef CREAD
#define CREAD    0x00080
#endif

#ifndef PARENB
#define PARENB   0x00100
#endif

#ifndef PARODD
#define PARODD   0x00200
#endif

#ifndef HPUCL
#define HUPCL    0x00400
#endif

#ifndef CLOCAL
#define CLOCAL   0x00800
#endif

#ifndef CBAUDEX
#define CBAUDEX  0x0100f
#endif

#ifndef B57600
#define B57600   0x01001
#endif

#ifndef B115200
#define B115200  0x01002
#endif

#ifndef B128000
#define B128000  0x01003
#endif

#ifndef B230400
#define B230400  0x01004
#endif

#ifndef B256000
#define B256000  0x01005
#endif

#ifndef CRTSXOFF
#define CRTSXOFF 0x04000
#endif

#ifndef CRTSCTS
#define CRTSCTS  0x08000
#endif

#ifndef ISIG
#define ISIG    0x0001
#endif

#ifndef ICANON
#define ICANON  0x0002
#endif

#ifndef ECHO
#define ECHO    0x0004
#endif

#ifndef ECHOE
#define ECHOE   0x0008
#endif

#ifndef ECHOK
#define ECHOK   0x0010
#endif

#ifndef ECHONL
#define ECHONL  0x0020
#endif

#ifndef NOFLSH
#define NOFLSH  0x0040
#endif

#ifndef TOSTOP
#define TOSTOP  0x0080
#endif

#ifndef IEXTEN
#define IEXTEN  0x0100
#endif

#ifndef FLUSHO
#define FLUSHO  0x0200
#endif

#ifndef ECHOKE
#define ECHOKE  0x0400
#endif

#ifndef ECHOCTL
#define ECHOCTL 0x0800
#endif

#ifndef TCOOFF
#define TCOOFF          0
#endif

#ifndef TCOON
#define TCOON           1
#endif

#ifndef TCIOFF
#define TCIOFF          2
#endif

#ifndef TCION
#define TCION           3
#endif

#ifndef TCIFLUSH
#define TCIFLUSH        0
#endif

#ifndef TCOFLUSH
#define TCOFLUSH        1
#endif

#ifndef TCIOFLUSH
#define TCIOFLUSH       2
#endif

#ifndef TCFLSH
#define TCFLSH          3
#endif

#ifndef TCSAFLUSH
#define TCSAFLUSH       1
#endif

#ifndef TCSANOW
#define TCSANOW         2
#endif

#ifndef TCSADRAIN
#define TCSADRAIN       3
#endif

#ifndef TCSADFLUSH
#define TCSADFLUSH      4
#endif

typedef unsigned char   MLton_cc_t;
typedef unsigned int    MLton_speed_t;
typedef unsigned int    MLton_tcflag_t;

#undef cc_t
#undef speed_t
#undef tcflag_t

#define cc_t MLton_cc_t
#define speed_t MLton_speed_t
#define tcflag_t MLton_tcflag_t

struct MLton_termios {
        cc_t c_cc[NCCS];
        tcflag_t c_cflag;
        tcflag_t c_iflag;
        tcflag_t c_lflag;
        tcflag_t c_oflag;
};
#undef termios
#define termios MLton_termios

MLTON_WRAPPER speed_t MLton_cfgetispeed (struct termios *termios_p);
MLTON_WRAPPER speed_t MLton_cfgetospeed (struct termios *termios_p);
MLTON_WRAPPER int MLton_cfsetispeed (struct termios *termios_p, speed_t speed);
MLTON_WRAPPER int MLton_cfsetospeed (struct termios *termios_p, speed_t speed);
MLTON_WRAPPER int MLton_tcdrain (int fd);
MLTON_WRAPPER int MLton_tcflow (int fd, int action);
MLTON_WRAPPER int MLton_tcflush (int fd, int queue_selector);
MLTON_WRAPPER int MLton_tcgetattr (int fd, struct termios *termios_p);
MLTON_WRAPPER pid_t MLton_tcgetpgrp (int fd);
MLTON_WRAPPER int MLton_tcsendbreak (int fd, int duration);
MLTON_WRAPPER int MLton_tcsetattr (int fd, int optional_actions, struct termios *termios_p);
MLTON_WRAPPER int MLton_tcsetpgrp (int fd, pid_t pgrpid);

#undef cfgetispeed
#undef cfgetospeed
#undef cfsetispeed
#undef cfsetospeed
#undef tcdrain
#undef tcflow
#undef tcflush
#undef tcgetattr
#undef tcgetpgrp
#undef tcsendbreak
#undef tcsetattr
#undef tcsetpgrp

#define cfgetispeed MLton_cfgetispeed
#define cfgetospeed MLton_cfgetospeed
#define cfsetispeed MLton_cfsetispeed
#define cfsetospeed MLton_cfsetospeed
#define tcdrain MLton_tcdrain
#define tcflow MLton_tcflow
#define tcflush MLton_tcflush
#define tcgetattr MLton_tcgetattr
#define tcgetpgrp MLton_tcgetpgrp
#define tcsendbreak MLton_tcsendbreak
#define tcsetattr MLton_tcsetattr
#define tcsetpgrp MLton_tcsetpgrp

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

// Unimplemented on windows:
#ifndef MSG_WAITALL
#define MSG_WAITALL 0x8
#endif

#ifndef MSG_DONTWAIT
#define MSG_DONTWAIT 0x1000000
#endif

#ifndef MSG_EOR
#define MSG_EOR 0
#endif

#ifndef MSG_CTRUNC
#define MSG_CTRUNC 0
#endif

#ifndef MSG_TRUNC
#define MSG_TRUNC MSG_PARTIAL
#endif

#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX 108
#endif

typedef unsigned short MLton_sa_family_t;
#undef sa_family_t
#define sa_family_t MLton_sa_family_t

struct MLton_sockaddr_un {
        sa_family_t sun_family;
        char sun_path[UNIX_PATH_MAX];
};
#undef sockaddr_un
#define sockaddr_un MLton_sockaddr_un

MLTON_WRAPPER int MLton_ioctl (int d, int request, ...);
MLTON_WRAPPER int MLton_socketpair (int d, int type, int protocol, int sv[2]);

#undef ioctl
#undef socketpair

#define ioctl MLton_ioctl
#define socketpair MLton_socketpair

/* ------------------------------------------------- */
/*                      Syslog                       */
/* ------------------------------------------------- */

#ifndef LOG_EMERG
#define LOG_EMERG 7
#endif

#ifndef LOG_ALERT
#define LOG_ALERT 6
#endif

#ifndef LOG_CRIT
#define LOG_CRIT 5
#endif

#ifndef LOG_ERR
#define LOG_ERR 4
#endif

#ifndef LOG_WARNING
#define LOG_WARNING 3
#endif

#ifndef LOG_NOTICE
#define LOG_NOTICE 2
#endif

#ifndef LOG_INFO
#define LOG_INFO 1
#endif

#ifndef LOG_DEBUG
#define LOG_DEBUG 0
#endif

#ifndef LOG_PID
#define LOG_PID    0x01 /* include PID in output */
#endif

#ifndef LOG_CONS
#define LOG_CONS   0x02 /* dump to console (meaningless for windows?) */
#endif

#ifndef LOG_ODELAY
#define LOG_ODELAY 0x04 /* delay open; meaningless---always open */
#endif

#ifndef LOG_NDELAY
#define LOG_NDELAY 0x08 /* don't delay; meaningless */
#endif

#ifndef LOG_NOWAIT
#define LOG_NOWAIT 0x10 /* ignored and obsolete anyways */
#endif

#ifndef LOG_PERROR
#define LOG_PERROR 0x20 /* print to standard error, honoured */
#endif

#ifndef LOG_AUTH
#define LOG_AUTH 1
#endif

#ifndef LOG_CRON
#define LOG_CRON 2
#endif

#ifndef LOG_DAEMON
#define LOG_DAEMON 3
#endif

#ifndef LOG_KERN
#define LOG_KERN 4
#endif

#ifndef LOG_LOCAL0
#define LOG_LOCAL0 5
#endif

#ifndef LOG_LOCAL1
#define LOG_LOCAL1 6
#endif

#ifndef LOG_LOCAL2
#define LOG_LOCAL2 7
#endif

#ifndef LOG_LOCAL3
#define LOG_LOCAL3 8
#endif

#ifndef LOG_LOCAL4
#define LOG_LOCAL4 9
#endif

#ifndef LOG_LOCAL5
#define LOG_LOCAL5 10
#endif

#ifndef LOG_LOCAL6
#define LOG_LOCAL6 11
#endif

#ifndef LOG_LOCAL7
#define LOG_LOCAL7 12
#endif

#ifndef LOG_LPR
#define LOG_LPR 13
#endif

#ifndef LOG_MAIL
#define LOG_MAIL 14
#endif

#ifndef LOG_NEWS
#define LOG_NEWS 15
#endif

#ifndef LOG_SYSLOG
#define LOG_SYSLOG 16
#endif

#ifndef LOG_USER
#define LOG_USER 17
#endif

#ifndef LOG_UUCP
#define LOG_UUCP 18
#endif

MLTON_WRAPPER void MLton_openlog(const char* ident, int logopt, int facility);
MLTON_WRAPPER void MLton_closelog(void);
MLTON_WRAPPER void MLton_syslog(int priority, const char* fmt, const char* msg);

#undef openlog
#undef closelog
#undef syslog

#define openlog MLton_openlog
#define closelog MLton_closelog
#define syslog MLton_syslog
