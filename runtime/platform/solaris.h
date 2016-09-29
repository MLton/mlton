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
/* This is defined in <sys/mman.h>, bet we'll redefine it in export.h. */
#undef PRIVATE
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

#ifdef __sparc__
#include "float-math.h"
#include "setenv.h"
#endif

#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "solaris"

#ifndef PRIxPTR
#define PRIxPTR "x"
#endif

/* Solaris 8 and older do not define a value for UINTPTR_MAX, so
   we redefine it with a value. */
#ifdef UINTPTR_MAX
#undef UINTPTR_MAX
#if defined(_LP64) || defined(_I32LPx)
#define UINTPTR_MAX     UINT64_MAX
#else
#define UINTPTR_MAX     UINT32_MAX
#endif
#endif


/* ------------------------------------------------- */
/*                  Posix.ProcEnv                    */
/* ------------------------------------------------- */

extern char **environ;


/* ------------------------------------------------- */
/*                  MLton.Syslog                     */
/* ------------------------------------------------- */

#define LOG_AUTHPRIV LOG_AUTH
#define LOG_PERROR 0


/* ------------------------------------------------- */
/*                         Real                      */
/* ------------------------------------------------- */

#ifdef __sparc__
#undef fegetround
#undef fesetround
#define fegetround MLton_fegetround
#define fesetround MLton_fesetround
int fegetround(void);
int fesetround(int rounding_mode);
#endif

#define FE_TONEAREST  0
#define FE_DOWNWARD   1
#define FE_UPWARD     2
#define FE_TOWARDZERO 3

#define HAS_FEROUND      TRUE

/* These are not predefined on Solaris 8. */
#ifndef NAN
#define NAN (__builtin_nanf (""))
#endif

#ifndef INFINITY
#define INFINITY (__builtin_inff())
#endif
