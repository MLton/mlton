#include "platform.h"

#if HAS_PTRACE

Int Ptrace_ptrace2 (Int request, Int pid) {
        return ptrace (request, pid, 0, 0);
}

Int Ptrace_ptrace4 (Int request, Int pid, Word addr, Pointer data) {
        return ptrace (request, pid, (int) addr, (int) data);
}

#else

/* We need the following to make the MacOS linker happy. */
int mlton_no_ptrace_dummy;

#endif
