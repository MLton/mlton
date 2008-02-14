#include <fenv.h>
#include <inttypes.h>
#include <stdint.h>

#include <unistd.h>

#include <windows.h> // lots of stuff depends on this

#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <utime.h>

#include <io.h>
#include <lm.h>
#include <process.h>
//#include <psapi.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <psapi.h>
#undef max

#define HAS_FEROUND TRUE
// As of 20051104, MinGW has fpclassify, but it is broken.  In particular, it
// classifies subnormals as normals.  So, we disable it here, which causes the
// runtime to use our own version.
#define HAS_FPCLASSIFY FALSE
#define HAS_FPCLASSIFY32 FALSE
#define HAS_FPCLASSIFY64 FALSE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP TRUE
#define HAS_SIGALTSTACK FALSE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN TRUE
#define HAS_TIME_PROFILING FALSE

#define MLton_Platform_OS_host "mingw"

typedef unsigned short gid_t;
typedef unsigned short uid_t;
typedef long suseconds_t; // type of timeval.tv_usec in sys/time.h
typedef short nlink_t; // type of st_nlink in sys/stat.h

// bullshit typedefs:
typedef unsigned int nfds_t; // we have a fake poll() with this many fds

int getpagesize (void);
int mkstemp (char *template);

#define POLLIN 1
#define POLLPRI 2
#define POLLOUT 4

#define _PC_CHOWN_RESTRICTED 6
#define _PC_LINK_MAX 0
#define _PC_MAX_CANON 1
#define _PC_MAX_INPUT 2
#define _PC_NAME_MAX 3
#define _PC_NO_TRUNC 7
#define _PC_PATH_MAX 4
#define _PC_PIPE_BUF 5
#define _PC_VDISABLE 8

#define F_DUPFD 0
#define F_GETFD 1
#define F_SETFD 2
#define F_GETFL 3
#define F_SETFL 4
#define F_GETOWN 5
#define F_SETOWN 6
#define F_GETLK 7
#define F_SETLK 8
#define F_RDLCK 1
#define F_WRLCK 2
#define F_UNLCK 3
#define F_SETLKW 9
#define FD_CLOEXEC 1

#define SHUT_RD SD_RECEIVE
#define SHUT_WR SD_SEND
#define SHUT_RDWR SD_BOTH

/* ------------------------------------------------- */
/*                       Date                        */
/* ------------------------------------------------- */

/* MinGW provides gettimeofday in -lmingwex, which we don't link.
 * In order to avoid a name conflict, we use a different name.
 */
int mlton_gettimeofday (struct timeval *tv, struct timezone *tz);
#define gettimeofday mlton_gettimeofday

/* ------------------------------------------------- */
/*                   MLton.Itimer                    */
/* ------------------------------------------------- */

#define ITIMER_REAL    0                /*generates sigalrm */  
#define ITIMER_VIRTUAL 1                /*generates sigvtalrm */
#define ITIMER_VIRT    1                /*generates sigvtalrm */
#define ITIMER_PROF    2                /*generates sigprof */ 

struct itimerval {
        struct timeval it_interval;
        struct timeval it_value;
};
int setitimer (int which,
                 const struct itimerval *value,
                 struct itimerval *ovalue);

/* ------------------------------------------------- */
/*                   MLton.Rlimit                    */
/* ------------------------------------------------- */

#define RLIMIT_CPU      0               /* CPU time in seconds */
#define RLIMIT_FSIZE    1               /* Maximum filesize */
#define RLIMIT_DATA     2               /* max data size */
#define RLIMIT_STACK    3               /* max stack size */
#define RLIMIT_CORE     4               /* max core file size */
#define RLIMIT_NOFILE   5               /* max number of open files */
#define RLIMIT_OFILE    RLIMIT_NOFILE   /* BSD name */
#define RLIMIT_AS       6               /* address space (virt. memory) limit */

#define RLIMIT_NLIMITS  7               /* upper bound of RLIMIT_* defines */
#define RLIM_NLIMITS    RLIMIT_NLIMITS

#define RLIM_INFINITY   (0xffffffffUL)
#define RLIM_SAVED_MAX  RLIM_INFINITY
#define RLIM_SAVED_CUR  RLIM_INFINITY

typedef unsigned long rlim_t;

struct rlimit {
        rlim_t  rlim_cur;
        rlim_t  rlim_max;
};

int getrlimit (int resource, struct rlimit *rlim);
int setrlimit (int resource, const struct rlimit *rlim);

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

#define RUSAGE_SELF 0               /* calling process */
#define RUSAGE_CHILDREN -1              /* terminated child processes */

struct rusage {
        struct timeval ru_utime;
        struct timeval ru_stime;
};

int getrusage (int who, struct rusage *usage);

/* ------------------------------------------------- */
/*                       OS.IO                       */
/* ------------------------------------------------- */

struct pollfd {
        short events;
        int fd;
        short revents;
};

int poll (struct pollfd *ufds, nfds_t nfds, int timeout);

/* ------------------------------------------------- */
/*                    Posix.Error                    */
/* ------------------------------------------------- */

#define EINPROGRESS WSAEINPROGRESS
#define EMSGSIZE WSAEMSGSIZE
#define ELOOP WSAELOOP
#define EBADMSG 77

/* ------------------------------------------------- */
/*                   Posix.FileSys                   */
/* ------------------------------------------------- */

#define S_IRGRP 0000040
#define S_IROTH 0000004
#define S_IRWXG 0000070
#define S_IRWXO 0000007
#define S_ISGID 0002000
#define S_ISUID 0004000
#define S_IWGRP 0000020
#define S_IWOTH 0000002
#define S_IXGRP 0000010
#define S_IXOTH 0000001

// Do not exist in a windows filesystem
#define S_IFLNK 0
#define S_IFSOCK 0
#define S_ISVTX 0

#define O_NOCTTY 0x8000
#define O_NONBLOCK 0x4000

// Synchronized writes? Safety of any kind? ... and windows?! hell no!
#define O_SYNC 0

// Use m to silence unused warnings
#define S_ISLNK(m) (m?FALSE:FALSE)
#define S_ISSOCK(m) (m?FALSE:FALSE)

#ifndef O_ACCMODE
#define O_ACCMODE O_RDONLY|O_WRONLY|O_RDWR
#endif

int chown (const char *path, uid_t owner, gid_t group);
int fchmod (int filedes, mode_t mode);
int fchdir (int filedes);
int fchown (int fd, uid_t owner, gid_t group);
long fpathconf (int filedes, int name);
int link (const char *oldpath, const char *newpath);
int lstat (const char *file_name, struct stat *buf);
int mkfifo (const char *pathname, mode_t mode);
long pathconf (const char *path, int name);
int readlink (const char *path, char *buf, size_t bufsiz);
int symlink (const char *oldpath, const char *newpath);
int truncate (const char *path, off_t len);

#define mkdir(f, m) mkdir(f); chmod(f, m)

/* ------------------------------------------------- */
/*                     Posix.IO                      */
/* ------------------------------------------------- */

struct flock {
        off_t l_len;
        pid_t l_pid;
        off_t l_start;
        short l_type;
        short l_whence;
};

int fcntl (int fd, int cmd, ...);
int fsync (int fd);
int pipe (int filedes[2]);

/* ------------------------------------------------- */
/*                   Posix.ProcEnv                   */
/* ------------------------------------------------- */

#define _SC_ARG_MAX 0
#define _SC_CHILD_MAX 1
#define _SC_CLK_TCK 2
#define _SC_JOB_CONTROL 5
#define _SC_NGROUPS_MAX 3
#define _SC_OPEN_MAX 4
#define _SC_SAVED_IDS 6
#define _SC_VERSION 7

struct tms {
        int tms_utime;
        int tms_stime;
        int tms_cutime;
        int tms_cstime;
};

struct utsname {
        char machine[20];
        char nodename[256];
        char release[20];
        char sysname[20];
        char version[20];
};

char *ctermid (char *s);
gid_t getegid (void);
uid_t geteuid (void);
gid_t getgid (void);
int getgroups (int size, gid_t list[]);
char *getlogin (void);
pid_t getpgid(pid_t pid);
pid_t getpgrp(void);
pid_t getppid (void);
uid_t getuid (void);
int setenv (const char *name, const char *value, int overwrite);
int setgid (gid_t gid);
int setgroups (size_t size, const gid_t *list);
int setpgid (pid_t pid, pid_t pgid);
pid_t setsid (void);
int setuid (uid_t uid);
long sysconf (int name);
clock_t times (struct tms *buf);
char *ttyname (int desc);
int uname (struct utsname *buf);

/* ------------------------------------------------- */
/*                   Posix.Process                   */
/* ------------------------------------------------- */

#define EXECVE(path, args, env)         \
        execve (path, (const char* const*)args, (const char* const*)env)
#define EXECVP(file, args)  execvp (file, (const char* const*) args)
#define SPAWN_MODE _P_NOWAIT

/* A status looks like:
      <2 bytes info> <2 bytes code>

      <code> == 0, child has exited, info is the exit value
      <code> == 1..7e, child has exited, info is the signal number.
      <code> == 7f, child has stopped, info was the signal number.
      <code> == 80, there was a core dump.
*/

#define WNOHANG 1
#define WUNTRACED 2
#define WIFEXITED(w)    (((w) & 0xff) == 0)
#define WIFSIGNALED(w)  (((w) & 0x7f) > 0 && (((w) & 0x7f) < 0x7f))
#define WIFSTOPPED(w)   (((w) & 0xff) == 0x7f)
#define WEXITSTATUS(w)  (((w) >> 8) & 0xff)
#define WTERMSIG(w)     ((w) & 0x7f)
#define WSTOPSIG        WEXITSTATUS

/* Sometimes defined by mingw */
#ifndef TIMESPEC_DEFINED
struct timespec {
  time_t tv_sec;
  long tv_nsec;
};
#endif

int alarm (int secs);
int fork(void); /* mingw demands this return int */
int kill (pid_t pid, int sig);
int pause (void);
int nanosleep (const struct timespec *req, struct timespec *rem);
unsigned int sleep (unsigned int seconds);
pid_t wait (int *status);
pid_t waitpid (pid_t pid, int *status, int options);

/* ------------------------------------------------- */
/*                   Posix.Signal                    */
/* ------------------------------------------------- */

#define SIG_BLOCK 1
#define SIG_SETMASK 0
#define SIG_UNBLOCK 2

/* Sometimes mingw defines some of these. Some not. Some always. */

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
#define SIGSTOP 16
#define SIGTSTP 18
#define SIGCHLD 23
#define SIGTTIN 24
#define SIGTTOU 25
#define SIGCONT 26
#define SIGUSR1 27
#define SIGUSR2 28
#define SIGVTALRM 29    /* virtual time alarm */
#define SIGPROF 30      /* profiling time alarm */

#define _NSIG 32

typedef __p_sig_fn_t _sig_func_ptr;
typedef int sigset_t; /* sometimes defined my mingw as int */

struct sigaction {
        int             sa_flags;
        sigset_t        sa_mask;
        _sig_func_ptr   sa_handler;
};

#define SIGTOMASK(sn)   (1 << ((sn)-1))

int sigaction (int signum, 
                        const struct sigaction *act, 
                        struct sigaction *oldact);
int sigaddset (sigset_t *set, int signum);
int sigdelset (sigset_t *set, int signum);
int sigemptyset (sigset_t *set);
int sigfillset (sigset_t *set);
int sigismember (const sigset_t *set, int signum);
int sigpending (sigset_t *set);
int sigprocmask (int how, const sigset_t *set, sigset_t *oldset);
int sigsuspend (const sigset_t *mask);

/* ------------------------------------------------- */
/*                Posix.SysDB.Passwd                 */
/* ------------------------------------------------- */

struct group {
        gid_t   gr_gid;
        char    **gr_mem;
        char    *gr_name;
        char    *gr_passwd;
};

struct passwd {
        char    *pw_dir;
        gid_t   pw_gid;
        char    *pw_name;
        char    *pw_shell;
        uid_t   pw_uid;
};

struct group *getgrgid (gid_t gid);
struct group *getgrnam (const char *name);
struct passwd *getpwnam (const char *name);
struct passwd *getpwuid (uid_t uid);

/* ------------------------------------------------- */
/*                     Posix.TTY                     */
/* ------------------------------------------------- */

#define B0       0x00000
#define B50      0x00001
#define B75      0x00002
#define B110     0x00003
#define B134     0x00004
#define B150     0x00005
#define B200     0x00006
#define B300     0x00007
#define B600     0x00008
#define B1200    0x00009
#define B1800    0x0000a
#define B2400    0x0000b
#define B4800    0x0000c
#define B9600    0x0000d
#define B19200   0x0000e
#define B38400   0x0000f
#define VEOL            2
#define VEOL2           3
#define VEOF            4
#define VERASE          5
#define VINTR           6
#define VKILL           7
#define VLNEXT          8
#define VMIN            9
#define VQUIT           10
#define VREPRINT        11
#define VSTART          12
#define VSTOP           13
#define VSUSP           14
#define VSWTC           15
#define VTIME           16
#define VWERASE 17

#define NCCS            18

#define IGNBRK  0x00001
#define BRKINT  0x00002
#define IGNPAR  0x00004
#define IMAXBEL 0x00008
#define INPCK   0x00010
#define ISTRIP  0x00020
#define INLCR   0x00040
#define IGNCR   0x00080
#define ICRNL   0x00100
#define IXON    0x00400
#define IXOFF   0x01000
#define IUCLC   0x04000
#define IXANY   0x08000
#define PARMRK  0x10000
#define OPOST   0x00001
#define CSIZE    0x00030
#define CS5      0x00000
#define CS6      0x00010
#define CS7      0x00020
#define CS8      0x00030
#define CSTOPB   0x00040
#define CREAD    0x00080
#define PARENB   0x00100
#define PARODD   0x00200
#define HUPCL    0x00400
#define CLOCAL   0x00800
#define CBAUDEX  0x0100f
#define B57600   0x01001
#define B115200  0x01002
#define B128000  0x01003
#define B230400  0x01004
#define B256000  0x01005
#define CRTSXOFF 0x04000
#define CRTSCTS  0x08000
#define ISIG    0x0001
#define ICANON  0x0002
#define ECHO    0x0004
#define ECHOE   0x0008
#define ECHOK   0x0010
#define ECHONL  0x0020
#define NOFLSH  0x0040
#define TOSTOP  0x0080
#define IEXTEN  0x0100
#define FLUSHO  0x0200
#define ECHOKE  0x0400
#define ECHOCTL 0x0800
#define TCOOFF          0
#define TCOON           1
#define TCIOFF          2
#define TCION           3
#define TCIFLUSH        0
#define TCOFLUSH        1
#define TCIOFLUSH       2
#define TCFLSH          3

#define TCSAFLUSH       1
#define TCSANOW         2
#define TCSADRAIN       3
#define TCSADFLUSH      4

typedef unsigned char   cc_t;
typedef unsigned int    speed_t;
typedef unsigned int    tcflag_t;

struct termios {
        cc_t c_cc[NCCS];
        tcflag_t c_cflag;
        tcflag_t c_iflag;
        tcflag_t c_lflag;
        tcflag_t c_oflag;
};

speed_t cfgetispeed (struct termios *termios_p);
speed_t cfgetospeed (struct termios *termios_p);
int cfsetispeed (struct termios *termios_p, speed_t speed);
int cfsetospeed (struct termios *termios_p, speed_t speed);
int tcdrain (int fd);
int tcflow (int fd, int action);
int tcflush (int fd, int queue_selector);
int tcgetattr (int fd, struct termios *termios_p);
pid_t tcgetpgrp (int fd);
int tcsendbreak (int fd, int duration);
int tcsetattr (int fd, int optional_actions, struct termios *termios_p);
int tcsetpgrp (int fd, pid_t pgrpid);

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

// Unimplemented on windows:
#ifndef MSG_WAITALL
#define MSG_WAITALL 0x8
#endif
#define MSG_DONTWAIT 0
#define MSG_EOR 0
#define MSG_CTRUNC 0

// Has a different name:
#define MSG_TRUNC MSG_PARTIAL


#define UNIX_PATH_MAX 108

typedef unsigned short  sa_family_t;

struct sockaddr_un {
        sa_family_t sun_family;
        char sun_path[UNIX_PATH_MAX];
};

int ioctl (int d, int request, ...);
int socketpair (int d, int type, int protocol, int sv[2]);

/* ------------------------------------------------- */
/*                      Syslog                       */
/* ------------------------------------------------- */

#define LOG_EMERG 7
#define LOG_ALERT 6
#define LOG_CRIT 5
#define LOG_ERR 4
#define LOG_WARNING 3
#define LOG_NOTICE 2
#define LOG_INFO 1
#define LOG_DEBUG 0

#define LOG_PID    0x01 /* include PID in output */
#define LOG_CONS   0x02 /* dump to console (meaningless for windows?) */
#define LOG_ODELAY 0x04 /* delay open; meaningless---always open */
#define LOG_NDELAY 0x08 /* don't delay; meaningless */
#define LOG_NOWAIT 0x10 /* ignored and obsolete anyways */
#define LOG_PERROR 0x20 /* print to standard error, honoured */

#define LOG_AUTH 1
#define LOG_CRON 2
#define LOG_DAEMON 3
#define LOG_KERN 4
#define LOG_LOCAL0 5
#define LOG_LOCAL1 6
#define LOG_LOCAL2 7
#define LOG_LOCAL3 8
#define LOG_LOCAL4 9
#define LOG_LOCAL5 10
#define LOG_LOCAL6 11
#define LOG_LOCAL7 12
#define LOG_LPR 13
#define LOG_MAIL 14
#define LOG_NEWS 15
#define LOG_SYSLOG 16
#define LOG_USER 17
#define LOG_UUCP 18

void openlog(const char* ident, int logopt, int facility);
void closelog(void);
void syslog(int priority, const char* fmt, const char* msg);

/* ------------------------------------------------- */
/*                      libdl                        */
/* ------------------------------------------------- */

void *dlopen(const char *filename, int flag_IGNORED);
const char *dlerror(void);
void *dlsym(void *void_hmodule, const char *symbol);
int dlclose(void *void_hmodule);
