#include "platform.h"

#if HAS_PTRACE

Int Ptrace_ptrace2 (Int request, Int pid) {
        return ptrace (request, pid, NULL, NULL);
}

Int Ptrace_ptrace4 (Int request, Int pid, Word addr, Pointer data) {
        return ptrace (request, pid, (void *) addr, (void *) data);
}

#endif
