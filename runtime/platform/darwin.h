#include <grp.h>
#include <limits.h>
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
#include <sys/sysctl.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <ucontext.h>

/* As far as I can tell, gmp does not come with Mac OS X, so the user will
 * install it himself in /usr/local.
 */
#include "/usr/local/include/gmp.h"

#define HAS_SPAWN FALSE
#define HAS_MREMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_TIME_PROFILING TRUE
#define HAS_WEAK 0
#define USE_MMAP TRUE

#define MLton_Platform_OS_host "darwin"
