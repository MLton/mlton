#include "platform.h"

#if HAS_PTRACE

Int Ptrace_ptrace2 (Int request, Int pid) {
	return ptrace (request, pid, 0, 0);
}

Int Ptrace_ptrace4 (Int request, Int pid, Word addr, Pointer data) {
	return ptrace (request, pid, (int) addr, (int) data);
}

#endif
