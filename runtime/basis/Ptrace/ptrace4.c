#if (defined (__linux__) || defined (__FreeBSD__))

#if (defined (__FreeBSD__))
#include <sys/types.h>
#endif
#include <sys/ptrace.h>

#include "mlton-basis.h"

Int Ptrace_ptrace4(Int request, Int pid, Word addr, Pointer data) {
	return ptrace(request, pid, (int) addr, (int) data);
}
#endif /* (defined (__linux__) || defined(__FreeBSD__)) */

