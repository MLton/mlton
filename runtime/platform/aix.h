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
#define HAS_FPCLASSIFY32 FALSE
#define HAS_FPCLASSIFY64 TRUE
#define HAS_MSG_DONTWAIT FALSE
#define HAS_PTRACE FALSE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING FALSE

#define MLton_Platform_OS_host "aix"

#define FE_TOWARDZERO 0 // FP_RND_RZ
#define FE_TONEAREST  1 // FP_RND_RN
#define FE_UPWARD     2 // FP_RND_RP
#define FE_DOWNWARD   3 // FP_RND_RM

int fegetround(void);
void fesetround(int mode);
int fpclassify64(double d);

#ifndef _AIXVERSION_520
/* These are GCC builtins, but <math.h> does not define the prototypes. */
float acosf(float x);
float asinf(float x);
float atan2f(float y, float x);
float atanf(float x);
float cosf(float x);
float coshf(float x);
float expf(float x);
float fabsf(float x);
float frexpf(float num, int *exp);
float ldexpf(float x, int exp);
float log10f(float x);
float logf(float x);
float modff(float x, float *iptr);
float powf(float x, float y);
float rintf(float x);
float sinf(float x);
float sinhf(float x);
float sqrtf(float x);
float tanf(float x);
float tanhf(float x);
#endif

/* This should not conflict with existing flags. */
#define MSG_DONTWAIT 0x1000000

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)-1)
#endif
