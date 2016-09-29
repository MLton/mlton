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

#undef UINTPTR_MAX
#define UINTPTR_MAX ULONG_MAX

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)SSIZE_MAX * 2 + 1)
#endif

#define HAS_FEROUND TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "hpux"

#define LOG_PERROR 0
#define LOG_AUTHPRIV LOG_AUTH

/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000

/* fesetround() doesn't seem to be returning 0 as expected. */
static int MLton_fesetround (int mode)
{
        fesetround (mode);
        return 0;
}
#define fesetround MLton_fesetround

typedef long suseconds_t; // type of timeval.tv_usec in sys/time.h

#ifdef __hppa__
/* These do not exist on HPPA, so we implement our own. */
float modff(float x, float *iptr);
float rintf(float x);
float frexpf(float x, int *exp);
float ldexpf(float x, int exp);
#endif /* __hppa__ */

#define PRIxPTR "lx"

/* These are incorrectly defined in the system headers. */
#undef PRIu32
#define PRIu32 "u"
#undef PRIx32
#define PRIx32 "x"
#undef PRId32
#define PRId32 "d"
