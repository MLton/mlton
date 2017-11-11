#include <inttypes.h>
#include <stdint.h>
#include <fenv.h>

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
#include <sys/sysinfo.h>
#include <syslog.h>
#include <termios.h>
#include <utime.h>

#define HAS_FEROUND TRUE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP TRUE
#define HAS_SIGALTSTACK TRUE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING FALSE

#define MLton_Platform_OS_host "hurd"

// environ is already defined if _GNU_SOURCE is.
#ifndef _GNU_SOURCE
extern char **environ; /* for Posix_ProcEnv_environ */
#endif
