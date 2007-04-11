#include <inttypes.h>

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <io.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <process.h>
#include <pwd.h>
#include <sys/cygwin.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <utime.h>
#include <windows.h>

#define MLton_Platform_OS_host "cygwin"

#define HAS_FEROUND FALSE
#define HAS_FPCLASSIFY TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK FALSE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN TRUE
#define HAS_TIME_PROFILING FALSE

/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000

/* Cygwin does not handle IPv6. */
#ifndef AF_INET6

#define AF_INET6 23 /* Internet Protocol, Version 6 */
#define PF_INET6 AF_INET6

struct sockaddr_in6 {
  int dummy; // quell gcc warnings about "struct has no members"
};

/* Cygwin does provide sockaddr_storage. */

#endif

typedef unsigned int nfds_t;
typedef long suseconds_t; // type of timeval.tv_usec in sys/time.h

// Unimplemented on Cygwin
#define MSG_WAITALL 0
#define MSG_EOR 0
