#include <grp.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <process.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <windows.h>
#include <sys/cygwin.h>
#include <io.h>

#define MLton_Platform_OS_host "cygwin"

#define HAS_FPCLASSIFY TRUE
#define HAS_FEROUND FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK FALSE
#define HAS_SIGNBIT TRUE
#define HAS_SPAWN TRUE
#define HAS_TIME_PROFILING FALSE

#define MSG_DONTWAIT 0
#define PF_INET6 0

struct sockaddr_in6 {};
