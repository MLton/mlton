#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

extern struct GC_state gcState;

Thread Thread_current() {
	return (Thread)gcState.currentThread;
}

void Thread_finishHandler() {
	GC_finishHandler (&gcState);
}

Thread Thread_saved() {
	Thread t;

	t = (Thread)gcState.savedThread;
	gcState.savedThread = (GC_thread)0x1;
	return t;
}

void Thread_setHandler(Thread t) {
 	gcState.signalHandler = (GC_thread)t;
}
