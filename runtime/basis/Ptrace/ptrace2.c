#include <sys/ptrace.h>
#include "mlton-basis.h"

Int Ptrace_ptrace2(Int request, Int pid) {
	return ptrace(request, pid, 0, 0);
}
