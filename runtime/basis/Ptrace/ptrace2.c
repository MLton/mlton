#if (defined (__linux__) || defined (__FreeBSD__))

#if (defined (__FreeBSD__))
#include <sys/types.h>
#endif
#include <sys/ptrace.h>

#include "mlton-basis.h"

Int Ptrace_ptrace2(Int request, Int pid) {
	return ptrace(request, pid, 0, 0);
}
#endif /* (defined (__linux__) || defined(__FreeBSD__)) */
