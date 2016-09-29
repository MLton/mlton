#if defined(_ARCH_PPC64)
#define __powerpc64__
#elif defined(_ARCH_PPC)
#define __powerpc__
#else
#error "Unknown architecture"
#endif

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <fenv.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/poll.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/syslog.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <termios.h>
#include <utime.h>

#define HAS_FEROUND TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_PTRACE FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING FALSE

#define MLton_Platform_OS_host "aix"

/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000
