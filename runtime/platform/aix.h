#define __ppc__

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
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
#define HAS_FPCLASSIFY FALSE
#define HAS_FPCLASSIFY64 TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_PTRACE FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING FALSE

#define MLton_Platform_OS_host "aix"

#include "feround.h"

#define FE_TOWARDZERO 0 // FP_RND_RZ
#define FE_TONEAREST  1 // FP_RND_RN
#define FE_UPWARD     2 // FP_RND_RP
#define FE_DOWNWARD   3 // FP_RND_RM

int fpclassify64(double d);

/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000
