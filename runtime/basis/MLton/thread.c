#include "platform.h"

extern struct GC_state gcState;

enum {
        DEBUG_THREAD = FALSE,
};

Thread Thread_current () {
        Thread t;

        t = (Thread)(GC_getCurrentThread (&gcState));
        if (DEBUG_THREAD)
                fprintf (stderr, "0x%08x = Thread_current ()\n", (uint)t);
        return t;
}

void Thread_finishHandler () {
        GC_finishHandler (&gcState);
}

Thread Thread_saved () {
        Thread t;

        t = (Thread)(GC_getSavedThread (&gcState));
        if (DEBUG_THREAD)
                fprintf (stderr, "0x%08x = Thread_saved ()\n", (uint)t);
        return t;
}

void Thread_setCallFromCHandler (Thread t) {
        GC_setCallFromCHandlerThread (&gcState, (GC_thread)t);
}

void Thread_setSaved (Thread t) {
        if (DEBUG_THREAD)
                fprintf (stderr, "Thread_setSaved (0x%08x)\n", (uint)t);
        GC_setSavedThread (&gcState, (GC_thread)t);
}

void Thread_setSignalHandler (Thread t) {
        GC_setSignalHandlerThread (&gcState, (GC_thread)t);
}

void Thread_startHandler () {
        GC_startHandler (&gcState);
}

void Thread_switchTo (Thread thread, Word ensureBytesFree) {
        if (DEBUG_THREAD)
                fprintf (stderr, "Thread_switchTo (0x%08x, %u)\n",
                                (uint)thread, (uint)ensureBytesFree);
        GC_switchToThread (&gcState, (GC_thread)thread, ensureBytesFree);
}
