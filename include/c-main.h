#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "main.h"
#include "c-common.h"

#define Main(al, cs, mg, mfs, mmc, pk, ps, mc, ml)			\
/* Globals */								\
int nextFun;								\
bool returnToC;								\
void MLton_callFromC () {						\
	struct cont cont;						\
	GC_state s;							\
									\
	if (DEBUG_CCODEGEN)						\
		fprintf (stderr, "MLton_callFromC() starting\n");	\
	s = &gcState;							\
	s->savedThread = s->currentThread;				\
	s->canHandle += 3;						\
	/* Switch to the C Handler thread. */				\
	GC_switchToThread (s, s->callFromCHandler, 0);			\
	nextFun = *(int*)(s->stackTop - WORD_SIZE);			\
	cont.nextChunk = nextChunks[nextFun];				\
	returnToC = FALSE;						\
	do {								\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
	} while (not returnToC);					\
	GC_switchToThread (s, s->savedThread, 0);      			\
 	s->savedThread = BOGUS_THREAD;					\
	if (DEBUG_CCODEGEN)						\
		fprintf (stderr, "MLton_callFromC done\n");		\
}									\
int main (int argc, char **argv) {					\
	struct cont cont;						\
	gcState.native = FALSE;						\
	Initialize (al, cs, mg, mfs, mmc, pk, ps);			\
	if (gcState.isOriginal) {					\
		real_Init();						\
		PrepFarJump(mc, ml);					\
	} else {							\
		/* Return to the saved world */				\
		nextFun = *(int*)(gcState.stackTop - WORD_SIZE);	\
		cont.nextChunk = nextChunks[nextFun];			\
	}								\
	/* Trampoline */						\
	while (1) {							\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
 		cont=(*(struct cont(*)(void))cont.nextChunk)();		\
	}								\
}

#endif /* #ifndef _C_MAIN_H */
