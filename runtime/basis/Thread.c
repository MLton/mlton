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

void Thread_switchTo (Thread thread) {
	GC_thread t;
	GC_state s;

	t = (GC_thread)thread;
	s = &gcState;
	s->currentThread->stack->used = s->stackTop - s->stackBottom;
	s->currentThread = t;
	s->stackBottom = ((pointer)t->stack) + sizeof(struct GC_stack); 
	s->stackTop = s->stackBottom + t->stack->used;
	s->stackLimit = 
		s->stackBottom + t->stack->reserved - 2 * s->maxFrameSize;
}
