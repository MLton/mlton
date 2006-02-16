#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <pwd.h>
#include <strings.h>
#include <sys/filio.h> /* For FIONBIO, FIONREAD. */
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/sockio.h> /* For SIOCATMARK. */
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <ucontext.h>

#include "setenv.h"

#define FE_TONEAREST 0
#define FE_DOWNWARD 1
#define FE_UPWARD 2
#define FE_TOWARDZERO 3
int fegetround ();
void fesetround (int mode);
int fpclassify64 (double d);

#define HAS_FEROUND TRUE
#define HAS_FPCLASSIFY FALSE
#define HAS_FPCLASSIFY64 TRUE
#define HAS_PTRACE TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "solaris"

#define LOG_AUTHPRIV LOG_AUTH
#define LOG_PERROR 0

int fpclassify32 (float f);
int fpclassify64 (double d);
int signbit32 (float f);
int signbit64 (double f);

