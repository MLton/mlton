#include <io.h>
#include <limits.h>
#include <lm.h>
#include <process.h>
#include <psapi.h>
#include <sys/stat.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#undef max

#include "gmp.h"
#include "resource.h"

#define HAS_MREMAP FALSE
#define HAS_SIGALTSTACK FALSE
#define HAS_WEAK FALSE
#define USE_VIRTUAL_ALLOC TRUE

int getpagesize (void);

struct timeval {
	long tv_sec;
	long tv_usec;
};

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
#define F_GETLK 7
#define F_SETLK 8
#define F_RDLCK 1
#define F_WRLCK 2
#define F_UNLCK 3
#define F_SETLKW 9
#define FD_CLOEXEC 1

#define MSG_DONTWAIT 0

#define SHUT_RD	SD_RECEIVE
#define SHUT_WR SD_SEND
#define SHUT_RDWR SD_BOTH

/* ------------------------------------------------- */
/*                       Date                        */
/* ------------------------------------------------- */

struct timezone {
    int tz_minuteswest;
    int tz_dsttime;
};

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
	rlim_t	rlim_cur;
	rlim_t	rlim_max;
};

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

#define RUSAGE_SELF 0               /* calling process */
#define RUSAGE_CHILDREN -1              /* terminated child processes */

struct rusage {
        struct timeval ru_utime;
        struct timeval ru_stime;
};

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

#define O_NOCTTY 0x8000
#define O_NONBLOCK 0x4000

#define S_ISLNK(m) 0

static inline int chmod (const char *path, mode_t mode) {
	return _chmod (path, mode);
}

static inline int mkdir (const char *pathname, mode_t mode) {
	return _mkdir (pathname, mode);
}

static inline int rmdir (const char *pathname) {
	return _rmdir (pathname);
}

static inline mode_t umask (mode_t mask) {
	return _umask (mask);
}

static inline int unlink (const char *pathname) {
	return _unlink (pathname);
}

int fchmod (int fildes, mode_t mode);
int lstat (const char *file_name, struct stat *buf);

/* ------------------------------------------------- */
/*                     Posix.IO                      */
/* ------------------------------------------------- */

int fcntl(int fd, int cmd);
int fcntl(int fd, int cmd, long arg);
int fcntl(int fd, int cmd, struct flock *lock);
int fsync (int fd);
int pipe (int filedes[2]);

struct flock {
	off_t l_len;
	pid_t l_pid;
	off_t l_start;
	short l_type;
	short l_whence;
};

/* ------------------------------------------------- */
/*                   Posix.ProcEnv                   */
/* ------------------------------------------------- */

#define _SC_BOGUS 0xFFFFFFFF
#define _SC_2_FORT_DEV _SC_BOGUS
#define _SC_2_FORT_RUN _SC_BOGUS
#define _SC_2_SW_DEV _SC_BOGUS
#define _SC_2_VERSION _SC_BOGUS
#define _SC_ARG_MAX 0
#define _SC_BC_BASE_MAX _SC_BOGUS
#define _SC_BC_DIM_MAX _SC_BOGUS
#define _SC_BC_SCALE_MAX _SC_BOGUS
#define _SC_BC_STRING_MAX _SC_BOGUS
#define _SC_CHILD_MAX 1
#define _SC_CLK_TCK 2
#define _SC_COLL_WEIGHTS_MAX _SC_BOGUS
#define _SC_EXPR_NEST_MAX _SC_BOGUS
#define _SC_JOB_CONTROL 5
#define _SC_LINE_MAX _SC_BOGUS
#define _SC_NGROUPS_MAX 3
#define _SC_OPEN_MAX 4
#define _SC_RE_DUP_MAX _SC_BOGUS
#define _SC_SAVED_IDS 6
#define _SC_STREAM_MAX _SC_BOGUS
#define _SC_TZNAME_MAX 20
#define _SC_VERSION 7

struct tms {	/* WARNING:  BOGUS! */
	int	tms_utime;		/* user time */
	int	tms_stime;		/* system time */
	int	tms_cutime;		/* user time, children */
	int	tms_cstime;		/* system time, children */
};

struct utsname {
	char machine[20];
	char nodename[20];
	char release[20];
	char sysname[20];
	char version[20];
};

int uname (struct utsname *buf);

/* ------------------------------------------------- */
/*                   Posix.Process                   */
/* ------------------------------------------------- */

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

int alarm (int secs);
pid_t fork (void);
pid_t wait (int *status);
pid_t waitpid (pid_t pid, int *status, int options);

/* ------------------------------------------------- */
/*                   Posix.Signal                    */
/* ------------------------------------------------- */

#define SIG_BLOCK 1
#define SIG_SETMASK 0
#define SIG_UNBLOCK 2

#define SIGHUP 1
#define SIGKILL 2
#define SIGPIPE 3
#define SIGQUIT 9
#define SIGALRM 13
#define SIGBUS 14
#define SIGSTOP 16
#define SIGTSTP 18
#define SIGCHLD 20
#define SIGTTIN 21
#define SIGTTOU 22
#define SIGCONT 25
#define SIGUSR1 25
#define SIGUSR2 26
#define SIGVTALRM 26	/* virtual time alarm */
#define SIGPROF 27	/* profiling time alarm */

#define _NSIG 32

typedef void (*_sig_func_ptr)();

struct sigaction {
	int		sa_flags;
	sigset_t	sa_mask;
	_sig_func_ptr	sa_handler;
};

#define SIGTOMASK(sn)	(1 << ((sn)-1))

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

struct passwd {
	char    *pw_dir;        /* home directory */
	gid_t   pw_gid;         /* group id */
	char    *pw_name;       /* user name */
	char    *pw_shell;      /* shell program */
	uid_t   pw_uid;         /* user id */
};

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

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

#define MSG_DONTWAIT 0
#define PF_INET6 0
struct sockaddr_in6 {};
struct sockaddr_un {};
