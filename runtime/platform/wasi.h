#define _GNU_SOURCE

#include <dirent.h>
#include <fcntl.h>
#include <fenv.h>
#include <inttypes.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/sysinfo.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <utime.h>

#define HAS_FEROUND TRUE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP FALSE
#define HAS_SHRINK_HEAP FALSE
#define HAS_SIGALTSTACK FALSE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING FALSE

#define MLton_Platform_OS_host "wasi"

/*
 * The definitions below are missing from WASI but are required by the runtime.
 *
 * In a small number of cases, these are implemented in wasi.c, but most of them
 * are not. See wasi.c for a discussion.
 *
 * These are not guarded by ifdefs so that we can easily detect when functionality
 * is added to WASI.
 *
 * This file is roughly based on the mingw implementation.
 */

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

#define ITIMER_REAL    0
#define ITIMER_VIRTUAL 1
#define ITIMER_PROF    2

struct itimerval {
        struct timeval it_interval;
        struct timeval it_value;
};

int setitimer (int, const struct itimerval *, struct itimerval *);

/* ------------------------------------------------- */
/*                        Rlimit                     */
/* ------------------------------------------------- */

#define RLIMIT_CPU      0
#define RLIMIT_FSIZE    1
#define RLIMIT_DATA     2
#define RLIMIT_STACK    3
#define RLIMIT_CORE     4
#define RLIMIT_NOFILE   5
#define RLIMIT_AS       6
#define RLIMIT_NLIMITS  7

#define RLIM_INFINITY   (0xffffffffUL)

typedef unsigned long rlim_t;

struct rlimit {
        rlim_t  rlim_cur;
        rlim_t  rlim_max;
};

int getrlimit (int resource, struct rlimit *);
int setrlimit (int resource, const struct rlimit *);

/* ------------------------------------------------- */
/*                    Networking                     */
/* ------------------------------------------------- */

struct servent {
        char *s_name;
        char **s_aliases;
        int s_port;
        char *s_proto;
};

struct servent *getservbyname (const char *, const char *);
extern struct servent *getservbyport (int, const char *);

struct hostent {
        char *h_name;
        char **h_aliases;
        int h_addrtype;
        int h_length;
        char **h_addr_list;
};

struct hostent *gethostbyaddr (const void *, socklen_t, int);
struct hostent *gethostbyname (const char *);

int socket (int, int, int);
int socketpair (int, int, int, int [2]);
int bind (int, const struct sockaddr *, socklen_t);
int connect (int, const struct sockaddr *, socklen_t);
int listen (int, int);
ssize_t recvfrom (int, void *, size_t, int, struct sockaddr *, socklen_t *);
ssize_t sendto (int, const void *, size_t, int, const struct sockaddr *, socklen_t);
int setsockopt (int, int, int, const void *, socklen_t);
int getpeername (int, struct sockaddr *, socklen_t *);
int gethostname(char *, size_t);
int getsockname (int, struct sockaddr *, socklen_t *);

#define SIOCATMARK      0x8905

#define SO_DEBUG        1
#define SO_REUSEADDR    2
#define SO_TYPE         3
#define SO_ERROR        4
#define SO_DONTROUTE    5
#define SO_BROADCAST    6
#define SO_SNDBUF       7
#define SO_RCVBUF       8
#define SO_KEEPALIVE    9
#define SO_OOBINLINE    10
#define SO_NO_CHECK     11
#define SO_PRIORITY     12
#define SO_LINGER       13
#define SO_BSDCOMPAT    14
#define SO_REUSEPORT    15
#define SO_PASSCRED     16
#define SO_PEERCRED     17
#define SO_RCVLOWAT     18
#define SO_SNDLOWAT     19
#define SO_ACCEPTCONN   30
#define SO_PEERSEC      31
#define SO_SNDBUFFORCE  32
#define SO_RCVBUFFORCE  33
#define SO_PROTOCOL     38
#define SO_DOMAIN       39
#define SO_RCVTIMEO     66
#define SO_SNDTIMEO     67

#define SOCK_RAW       3
#define SOCK_RDM       4
#define SOCK_SEQPACKET 5
#define SOCK_DCCP      6
#define SOCK_PACKET    10

struct sockaddr_un {
        sa_family_t sun_family;
        char sun_path[108];
};

#define MSG_OOB       0x0001
#define MSG_DONTROUTE 0x0004
#define MSG_CTRUNC    0x0008
#define MSG_PROXY     0x0010
#define MSG_DONTWAIT  0x0040
#define MSG_EOR       0x0080

struct protoent {
  char *p_name;
  char **p_aliases;
  int p_proto;
};

struct protoent *getprotobyname (const char *);
struct protoent *getprotobynumber (int);

/* ------------------------------------------------- */
/*                       Files                       */
/* ------------------------------------------------- */

#define F_DUPFD  0
#define F_GETOWN 5
#define F_SETOWN 6
#define F_GETLK  7
#define F_SETLK  8

#define F_RDLCK 1
#define F_WRLCK 2
#define F_UNLCK 3

#define F_SETLKW 9

int chmod(const char *, mode_t);
int chown (const char *, uid_t, gid_t);
int fchdir (int);
int fchmod (int, mode_t);
int fchown (int, uid_t, gid_t);
long fpathconf (int, int);
int link (const char *, const char *);
int lstat (const char *, struct stat *);
int mkfifo (const char *, mode_t);
int mkstemp (char *);
long pathconf (const char *, int);
int symlink (const char *, const char *);
int truncate (const char *, off_t);
mode_t umask(mode_t);

int dup(int);
int dup2(int, int);
int fcntl (int, int, ...);
int fsync (int);
int pipe (int [2]);

/* ------------------------------------------------- */
/*                    Processes                      */
/* ------------------------------------------------- */

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
        char sysname[30];
        char version[20];
};

char *getlogin (void);
char *ctermid (char *);
gid_t getegid (void);
uid_t geteuid (void);
gid_t getgid (void);
int getgroups (int, gid_t []);
pid_t getpgid(pid_t);
pid_t getpgrp(void);
pid_t getppid (void);
uid_t getuid (void);
int setenv (const char *, const char *, int);
int setgid (gid_t);
int setgroups (size_t, const gid_t *);
int setpgid (pid_t, pid_t);
pid_t setsid (void);
int setuid (uid_t);
clock_t times (struct tms *);
char *ttyname (int);
int uname (struct utsname *);

#define POLLPRI 0x002

#define WNOHANG   1
#define WUNTRACED 2

#define WIFEXITED(w)    ((w) & 0)
#define WIFSIGNALED(w)  ((w) & 0)
#define WIFSTOPPED(w)   ((w) & 0)
#define WTERMSIG(w)     ((w) & 0)
#define WEXITSTATUS(w)  ((w) & 0)
#define WSTOPSIG        WEXITSTATUS

int alarm(int);
int execve(const char *, char *const [], char *const []);
int execvp(const char *, char *const []);
pid_t fork(void);
int kill(pid_t, int);
int pause(void);
pid_t waitpid (pid_t, int *, int);

/* ------------------------------------------------- */
/*                      Signals                      */
/* ------------------------------------------------- */

#define SIG_BLOCK     0
#define SIG_UNBLOCK   1
#define SIG_SETMASK   2

struct sigaction {
        int             sa_flags;
        sigset_t        sa_mask;
        void (*sa_handler)(int);
};

int sigaction (int, const struct sigaction *, struct sigaction *);
int sigaddset (sigset_t *, int);
int sigdelset (sigset_t *, int);
int sigemptyset (sigset_t *);
int sigfillset (sigset_t *);
int sigismember (const sigset_t *, int);
int sigpending (sigset_t *);
int sigprocmask (int, const sigset_t *, sigset_t *);
int sigsuspend (const sigset_t *);

/* ------------------------------------------------- */
/*                       Passwd                      */
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

struct group *getgrgid (gid_t);
struct group *getgrnam (const char *);
struct passwd *getpwnam (const char *);
struct passwd *getpwuid (uid_t);

/* ------------------------------------------------- */
/*                        TTY                        */
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
#define VWERASE         17
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

typedef unsigned char cc_t;
typedef unsigned int speed_t;
typedef unsigned int tcflag_t;

struct termios {
        cc_t c_cc[NCCS];
        tcflag_t c_cflag;
        tcflag_t c_iflag;
        tcflag_t c_lflag;
        tcflag_t c_oflag;
};

speed_t cfgetispeed (struct termios *);
speed_t cfgetospeed (struct termios *);
int cfsetispeed (struct termios *, speed_t);
int cfsetospeed (struct termios *, speed_t);
int tcdrain (int);
int tcflow (int, int);
int tcflush (int, int);
int tcgetattr (int, struct termios *);
pid_t tcgetpgrp (int);
int tcsendbreak (int, int);
int tcsetattr (int, int, struct termios *);
int tcsetpgrp (int, pid_t);

/* ------------------------------------------------- */
/*                      Syslog                       */
/* ------------------------------------------------- */

#define LOG_DEBUG   0
#define LOG_INFO    1
#define LOG_NOTICE  2
#define LOG_WARNING 3
#define LOG_ERR     4
#define LOG_CRIT    5
#define LOG_ALERT   6
#define LOG_EMERG   7

#define LOG_PID    0x01
#define LOG_CONS   0x02
#define LOG_ODELAY 0x04
#define LOG_NDELAY 0x08
#define LOG_NOWAIT 0x10
#define LOG_PERROR 0x20

#define LOG_AUTH   1
#define LOG_CRON   2
#define LOG_DAEMON 3
#define LOG_KERN   4
#define LOG_LOCAL0 5
#define LOG_LOCAL1 6
#define LOG_LOCAL2 7
#define LOG_LOCAL3 8
#define LOG_LOCAL4 9
#define LOG_LOCAL5 10
#define LOG_LOCAL6 11
#define LOG_LOCAL7 12
#define LOG_LPR    13
#define LOG_MAIL   14
#define LOG_NEWS   15
#define LOG_SYSLOG 16
#define LOG_USER   17
#define LOG_UUCP   18

void openlog (const char *, int, int);
void closelog (void);
void syslog (int, const char *, const char *);
