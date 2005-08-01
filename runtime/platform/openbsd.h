#include <grp.h>
#include <inttypes.h>
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

#define HAS_FEROUND FALSE
#define HAS_FPCLASSIFY FALSE
#define HAS_PTRACE FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE
#define HAS_WEAK FALSE

#define MLton_Platform_OS_host "openbsd"

int fpclassify32 (float f);
int fpclassify64 (double d);
int signbit32 (float f);
int signbit64 (double f);
