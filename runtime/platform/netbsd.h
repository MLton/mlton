#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/times.h>

/* On NetBSD, we want gmp to be installed into the pkg tree (which represents
 * the FreeBSD ports tree). For now we use the same method as in the FreeBSD
 * case, but we note that this should be changed so the makefile provides the
 * correct -I flags to the compiler
 */
#include "/usr/pkg/include/gmp.h"

#include "ptrace.h"
#include "spawn.h"

#define HAS_MREMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_TIME_PROFILING TRUE
#define HAS_WEAK 1
#define USE_MMAP TRUE
