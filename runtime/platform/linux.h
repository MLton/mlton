#include <inttypes.h>
#include <stdint.h>
#ifdef __UCLIBC__
#include <fpu_control.h>
#else
#include <fenv.h>
#endif

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
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

#ifdef __UCLIBC__
#define HAS_FEROUND FALSE
#else
#define HAS_FEROUND TRUE
#endif
#define HAS_FPCLASSIFY TRUE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP TRUE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "linux"

// environ is already defined if _GNU_SOURCE is.
#ifndef _GNU_SOURCE
extern char **environ; /* for Posix_ProcEnv_environ */
#endif

/* The following is compatibility code with older glibc and kernel
   versions. */

#ifndef __suseconds_t_defined
#include <linux/types.h>
typedef __kernel_suseconds_t suseconds_t;
#define __suseconds_t_defined
#endif

#if __GLIBC__ == 2 && __GLIBC_MINOR__ <= 1
typedef unsigned long int nfds_t;
#endif

#ifndef SO_ACCEPTCONN
#define SO_ACCEPTCONN 30
#endif

#ifdef __UCLIBC__
#define FE_DOWNWARD     _FPU_RC_DOWN
#define FE_TONEAREST    _FPU_RC_NEAREST
#define FE_TOWARDZERO   _FPU_RC_ZERO
#define FE_UPWARD       _FPU_RC_UP
#endif
