#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
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
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <ucontext.h>

#include "setenv.h"

#define HAS_PTRACE TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE
#define HAS_WEAK 1

#define MLton_Platform_OS_host "solaris"

#define LOG_AUTHPRIV LOG_AUTH
#define LOG_PERROR 0
