/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */

#ifndef _BYTECODE_MAIN_H_
#define _BYTECODE_MAIN_H_

#include "main.h"
#include "interpret.h"

#ifndef DEBUG_CODEGEN
#define DEBUG_CODEGEN FALSE
#endif

struct Bytecode MLton_bytecode;

static Word32 returnAddressToFrameIndex (Word32 w) {
	return *(Word32*)(MLton_bytecode.code + w - sizeof (Word32));
}

#define Main(al, mg, mfs, mmc, pk, ps, ml)				\
void MLton_callFromC () {						\
	int nextFun;							\
	GC_state s;							\
									\
	if (DEBUG_CODEGEN)						\
		fprintf (stderr, "MLton_callFromC() starting\n");	\
	s = &gcState;							\
	s->savedThread = s->currentThread;				\
	s->canHandle += 3;						\
	/* Switch to the C Handler thread. */				\
	GC_switchToThread (s, s->callFromCHandler, 0);			\
	nextFun = *(int*)(s->stackTop - WORD_SIZE);			\
	MLton_Bytecode_interpret (&MLton_bytecode, nextFun);		\
	GC_switchToThread (s, s->savedThread, 0);			\
 	s->savedThread = BOGUS_THREAD;					\
	if (DEBUG_CODEGEN)						\
		fprintf (stderr, "MLton_callFromC done\n");		\
}									\
int main (int argc, char **argv) {					\
	int nextFun;							\
	Initialize (al, mg, mfs, mmc, pk, ps);				\
	if (gcState.isOriginal) {					\
		real_Init();						\
		nextFun = ml;						\
	} else {							\
		/* Return to the saved world */				\
		nextFun = *(int*)(gcState.stackTop - WORD_SIZE);	\
	}								\
	MLton_Bytecode_interpret (&MLton_bytecode, nextFun);		\
}

#endif /* #ifndef _BYTECODE_MAIN_H */
