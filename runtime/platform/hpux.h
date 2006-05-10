#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif

#include <inttypes.h>
#include <math.h>
#include <signal.h>
#include <poll.h>
#include <termios.h>
#include <sys/resource.h>
#include <sys/ptrace.h>
#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/times.h>
#include <sys/utsname.h>
#include <termios.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <grp.h>
#include <fenv.h>
#include <syslog.h>

#include "setenv.h"

#define HAS_FEROUND TRUE
#define HAS_FPCLASSIFY TRUE
#define HAS_PTRACE FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "hpux"

#define LOG_PERROR 0
#define LOG_AUTHPRIV LOG_AUTH

#define MSG_DONTWAIT 0

#ifndef PF_INET6
/* Old versions of HP-UX don't have IPv6 support. */
struct sockaddr_in6 {};
#define PF_INET6 0
#endif
