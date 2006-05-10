#include <stdint.h>

#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <pwd.h>
#include <poll.h>
#include <termios.h>
#include <sys/resource.h>
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

#define HAS_FEROUND FALSE
#define HAS_FPCLASSIFY TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "netbsd"
