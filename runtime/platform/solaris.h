#include <gmp.h>
#include <signal.h>
#include <strings.h>
#include <sys/filio.h> /* For FIONBIO, FIONREAD. */
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/sockio.h> /* For SIOCATMARK. */
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <ucontext.h>

#include "setenv.h"
#include "spawn.h"

#define HAS_MREMAP FALSE
#define HAS_TIME_PROFILING TRUE
#define HAS_SIGALTSTACK TRUE
#define HAS_WEAK 1
#define USE_MMAP TRUE

#define LOG_AUTHPRIV LOG_AUTH
#define LOG_PERROR 0
