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

#include "feround.h"
#define signbit32 signbit
#define signbit64 signbit
#define fpclassify32 fpclassify
#define fpclassify64 fpclassify

#define MLton_Platform_OS_host "cygwin"

#define HAS_PTRACE FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK FALSE
#define HAS_SPAWN TRUE
#define HAS_TIME_PROFILING FALSE
#define HAS_WEAK FALSE

#define _SC_BOGUS 0xFFFFFFFF
#define _SC_2_FORT_DEV _SC_BOGUS
#define _SC_2_FORT_RUN _SC_BOGUS
#define _SC_2_SW_DEV _SC_BOGUS
#define _SC_2_VERSION _SC_BOGUS
#define _SC_BC_BASE_MAX _SC_BOGUS
#define _SC_BC_DIM_MAX _SC_BOGUS
#define _SC_BC_SCALE_MAX _SC_BOGUS
#define _SC_BC_STRING_MAX _SC_BOGUS
#define _SC_COLL_WEIGHTS_MAX _SC_BOGUS
#define _SC_EXPR_NEST_MAX _SC_BOGUS
#define _SC_LINE_MAX _SC_BOGUS
#define _SC_RE_DUP_MAX _SC_BOGUS
#define _SC_STREAM_MAX _SC_BOGUS

#define MSG_DONTWAIT 0
#define PF_INET6 0

struct sockaddr_in6 {};
