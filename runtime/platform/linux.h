#include <gmp.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/sysinfo.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <values.h>

#define HAS_PTRACE TRUE
#define HAS_REMAP TRUE
#define HAS_SIGALTSTACK TRUE
#define HAS_SPAWN FALSE
#ifdef __i386__
#define HAS_TIME_PROFILING TRUE
#else
#define HAS_TIME_PROFILING FALSE
#endif
#define HAS_WEAK 1

#define MLton_Platform_OS_host "linux"


