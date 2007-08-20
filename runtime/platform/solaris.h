#include <inttypes.h>

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <poll.h>
#include <pwd.h>
#include <strings.h>
#include <sys/filio.h> /* For FIONBIO, FIONREAD. */
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/sockio.h> /* For SIOCATMARK. */
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <ucontext.h>
#include <unistd.h>
#include <utime.h>

#include "feround.h"
#include "float-math.h"
#include "setenv.h"

#define FE_TONEAREST 0
#define FE_DOWNWARD 1
#define FE_UPWARD 2
#define FE_TOWARDZERO 3

#define HAS_FEROUND TRUE
#define HAS_FPCLASSIFY FALSE
#define HAS_FPCLASSIFY32 FALSE
#define HAS_FPCLASSIFY64 TRUE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "solaris"

#define LOG_AUTHPRIV LOG_AUTH
#define LOG_PERROR 0

int fpclassify32 (float f);
int fpclassify64 (double d);

#ifndef PRIuPTR
#define PRIuPTR "u"
#endif

#ifndef PRIxPTR
#define PRIxPTR "x"
#endif

extern char **environ; /* for Posix_ProcEnv_environ */

/* Solaris 7 does not define MAP_ANON. */

#ifndef MAP_ANON
#define MAP_ANON        0x100           /* map anonymous pages directly */
#endif

/* Solaris 7 does not handle IPv6. */
#ifndef AF_INET6

#define AF_INET6 26 /* Internet Protocol, Version 6 */
#define PF_INET6 AF_INET6

struct sockaddr_in6 {
  int dummy; // quell gcc warnings about "struct has no members"
};
struct sockaddr_storage {
  union {
    struct sockaddr_in sa_in;
    struct sockaddr_un sa_un;
  } sa;
}

#endif
