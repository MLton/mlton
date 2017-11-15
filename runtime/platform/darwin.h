#include <fenv.h>
#include <inttypes.h>
#include <stdint.h>

#include <unistd.h>

#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <sys/ptrace.h>
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
#include <sys/ucontext.h>
#include <utime.h>

#include <crt_externs.h>

#define HAS_FEROUND TRUE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define NEEDS_SIGALTSTACK_EXEC FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE

#define MLton_Platform_OS_host "darwin"

// MacOS only defines this if POSIX_C_SOURCE is defined.
// However, defining that breaks half the osx system headers.
// They couldn't possibly change the number at this point anyway.
#ifndef SIGPOLL
#define SIGPOLL 7
#endif

/* for Posix_ProcEnv_environ */
#define environ *_NSGetEnviron()
