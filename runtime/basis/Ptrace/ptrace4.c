#if (defined (__linux__))
#include "mlton-basis.h"

#include <sys/ptrace.h>

Int Ptrace_ptrace4(Int request, Int pid, Word addr, Pointer data) {
	return ptrace(request, pid, (int) addr, (int) data);
}
#endif /* (defined (__linux__)) */
