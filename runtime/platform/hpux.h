#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif

#include <fenv.h>
#include <inttypes.h>

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <signal.h>
#include <sys/poll.h>
#include <sys/privgrp.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <termios.h>
#include <utime.h>

#include "setenv.h"

#if defined(SO_TOE)
#define HPUX_VERSION 1123
#elif defined(PRIV_PSET)
#define HPUX_VERSION 1111
#elif defined(PRIV_SPUCTL)
#define HPUX_VERSION 1100
#else
#error "HP-UX 11.00 is the oldest supported version."
#endif

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

/* Old versions of HP-UX do not handle IPv6. */
#ifndef AF_INET6

#define AF_INET6 22 /* Internet Protocol, Version 6 */
#define PF_INET6 AF_INET6

struct sockaddr_in6 {
  int dummy; // quell gcc warnings about "struct has no members"
};
#endif /* !AF_INET6 */

#if HPUX_VERSION <= 1111
struct sockaddr_storage {
  union {
    struct sockaddr_in sa_in;
    struct sockaddr_un sa_un;
  } sa;
};
#endif /* HPUX_VERSION <= 1111 */

typedef long suseconds_t; // type of timeval.tv_usec in sys/time.h

#ifdef __hppa__
/* These do not exist on HPPA, so we implement our own. */
float modff(float x, float *iptr);
float rintf(float x);
float frexpf(float x, int *exp);
float ldexpf(float x, int exp);
#endif /* __hppa__ */

#define PRIxPTR "lx"
#define PRIuPTR "lu"

/* These are incorrectly defined in the system headers. */
#undef PRIu32
#define PRIu32 "u"
#undef PRIx32
#define PRIx32 "x"
#undef PRId32
#define PRId32 "d"

#define SIZE_MAX ((size_t)SSIZE_MAX * 2 + 1)
