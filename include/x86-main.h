#ifndef _X86_MAIN_H_
#define _X86_MAIN_H_

#include "main.h"

/* Globals */
word applyFFTemp;
word checkTemp;
char cReturnTempB;
double cReturnTempD;
word cReturnTempL;
word c_stackP;
word divTemp;
word fileTemp;
word fpswTemp;
word indexTemp;
word intInfTemp;
char MLton_bug_msg[] = "cps machine";
word raTemp1;
double raTemp2;
double realTemp1;
double realTemp2;
double realTemp3;
word spill[16];
word stackTopTemp;
word statusTemp;
word switchTemp;
word threadTemp;

#ifndef DEBUG_X86CODEGEN
#define DEBUG_X86CODEGEN FALSE
#endif

#if (defined (__CYGWIN__))
#define ReturnToC "_Thread_returnToC"
#elif (defined (__FreeBSD__) || defined (__linux__) || defined (__sun__))
#define ReturnToC "Thread_returnToC"
#else
#error ReturnToC not defined
#endif

#define Main(al, cs, mg, mfs, mlw, mmc, ps, ml, reserveEsp)		\
void MLton_jumpToSML (pointer jump) {					\
	word lc_stackP;							\
			       						\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "MLton_jumpToSML(0x%08x) starting\n", (uint)jump); \
	lc_stackP = c_stackP;						\
	if (reserveEsp)							\
		__asm__ __volatile__					\
		("pusha\nmovl %%esp,%0\nmovl %1,%%ebp\nmovl %2,%%edi\njmp *%3\n.global "ReturnToC"\n"ReturnToC":\nmovl %0,%%esp\npopa" \
		: "=o" (c_stackP)					\
		: "o" (gcState.stackTop), "o" (gcState.frontier), "r" (jump) \
		);							\
	else								\
		__asm__ __volatile__ 					\
		("pusha\nmovl %%esp,%0\nmovl %1,%%ebp\nmovl %2,%%esp\njmp *%3\n.global "ReturnToC"\n"ReturnToC":\nmovl %0,%%esp\npopa" \
		: "=o" (c_stackP)					\
		: "o" (gcState.stackTop), "o" (gcState.frontier), "r" (jump) \
		);							\
	c_stackP = lc_stackP;						\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "MLton_jumpToSML(0x%08x) done\n", (uint)jump); \
	return;								\
}									\
void MLton_callFromC () {						\
	pointer jump;							\
	GC_state s;							\
									\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "MLton_callFromC() starting\n");	\
	s = &gcState;							\
	s->savedThread = s->currentThread;				\
	s->canHandle++;							\
	/* Return to the C Handler thread. */				\
	GC_switchToThread (s, s->callFromCHandler);			\
	jump = *(pointer*)(s->stackTop - WORD_SIZE);			\
	MLton_jumpToSML(jump);						\
	GC_switchToThread (s, s->savedThread);				\
	s->canHandle--;							\
	s->savedThread = BOGUS_THREAD;					\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "MLton_callFromC() done\n");		\
	return;								\
}									\
int main (int argc, char **argv) {					\
	pointer jump;  							\
	extern pointer ml;						\
	gcState.native = TRUE;						\
	Initialize (al, cs, mg, mfs, mlw, mmc, ps);			\
	if (gcState.isOriginal) {					\
		real_Init();						\
		jump = (pointer)&ml;   					\
	} else {       							\
		jump = *(pointer*)(gcState.stackTop - WORD_SIZE); 	\
	}								\
	MLton_jumpToSML(jump);						\
	return 1;							\
}

#endif /* #ifndef _X86_MAIN_H_ */

