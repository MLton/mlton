#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>

/* On NetBSD, we want gmp to be installed into the pkg tree (which represents
 * the FreeBSD ports tree). For now we use the same method as in the FreeBSD
 * case, but we note that this should be changed so the makefile provides the
 * correct -I flags to the compiler
 */
#include "/usr/pkg/include/gmp.h"

#define HAS_MREMAP FALSE
#define HAS_PTRACE FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE
#define HAS_WEAK 1
#define USE_MMAP TRUE
