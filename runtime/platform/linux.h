#include <gmp.h>
#include <sys/ptrace.h>
#include <sys/sysinfo.h>
#include <ucontext.h>
#include <values.h>

#include "spawn.h"

#define HAS_MREMAP TRUE
#define HAS_SIGALTSTACK TRUE
#define HAS_TIME_PROFILING TRUE
#define HAS_WEAK 1
#define USE_MMAP TRUE

/* We need the value of MREMAP_MAYMOVE, which should come from sys/mman.h, but
 * isn't there.  It is in linux/mman.h, but we can't #include that here, because
 * kernel headers don't mix with system headers.  We could create a separate
 * file, include the kernel headers there, and define a global.  But there
 * sometimes seem to be problems including kernel headers, so the easiest thing
 * to do is just define MREMAP_MAYMOVE.
 */
#define MREMAP_MAYMOVE 1

