#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif

#include <fenv.h>
#include <grp.h>
#include <inttypes.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <signal.h>
#include <sys/poll.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <termios.h>
#include <termios.h>

#include "setenv.h"

#define HAS_FEROUND TRUE
#define HAS_FPCLASSIFY TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "hpux"

#define LOG_PERROR 0
#define LOG_AUTHPRIV LOG_AUTH

/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000

#ifndef PF_INET6
/* Old versions of HP-UX don't have IPv6 support. */
struct sockaddr_in6 { char dummy; };
#define sockaddr_storage sockaddr_in
#define PF_INET6 0
#define AF_INET6 0
#endif

typedef long suseconds_t;

/* These GCC builtins aren't defined in the system headers. */
float modff(float x, float *iptr);
float rintf(float x);
float frexpf(float x, int *exp);
float ldexpf(float x, int exp);

#define PRIxPTR "lx"

/* These are incorrectly defined in the system headers. */
#undef PRIu32
#define PRIu32 "u"
#undef PRIx32
#define PRIx32 "x"
#undef PRId32
#define PRId32 "d"

#define SIZE_MAX ((size_t)SSIZE_MAX * 2 + 1)
