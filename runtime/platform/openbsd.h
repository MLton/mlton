#include <inttypes.h>
#include <stdint.h>

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/sysctl.h>
#include <sys/time.h>
#include <sys/resource.h> /* <sys/resource.h> might not #include <sys/time.h> */
#include <sys/times.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <syslog.h>
#include <termios.h>
#include <utime.h>

#define HAS_FEROUND FALSE
#define HAS_FPCLASSIFY FALSE
#define HAS_FPCLASSIFY32 FALSE
#define HAS_FPCLASSIFY64 FALSE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "openbsd"

int fpclassify32 (float f);
int fpclassify64 (double d);

/* #ifndef PRIu8 */
/* #define PRIu8 "hhu" */
/* #endif */
/* #ifndef PRIu16 */
/* #define PRIu16 "hu" */
/* #endif */
/* #ifndef PRIx16 */
/* #define PRIx16 "hx" */
/* #endif */
/* #ifndef PRId32 */
/* #define PRId32 "d" */
/* #endif */
/* #ifndef PRIu32 */
/* #define PRIu32 "u" */
/* #endif */
/* #ifndef PRIx32 */
/* #define PRIx32 "x" */
/* #endif */
/* typedef long long int intmax_t; */
/* #ifndef INTMAX_MIN */
/* #define INTMAX_MIN LLONG_MIN */
/* #endif */
/* typedef unsigned long long int uintmax_t; */
/* #ifndef PRIuMAX */
/* #define PRIuMAX "llu" */
/* #endif */
/* #ifndef PRIxMAX */
/* #define PRIxMAX "llx" */
/* #endif */

/* #ifndef PRIxPTR */
/* #define PRIxPTR "lx" */
/* #endif */

extern char **environ; /* for Posix_ProcEnv_environ */
