#ifndef _X86_MAIN_H_
#define _X86_MAIN_H_

#include "main.h"

/* Globals */
Word applyFFTemp;
Word applyFFTemp2;
Word checkTemp;
Word cReturnTemp[16];
Word c_stackP;
Word divTemp;
Word eq1Temp;
Word eq2Temp;
Word fileTemp;
Word fildTemp;
Word fpswTemp;
Word indexTemp;
Word intInfTemp;
char MLton_bug_msg[] = "cps machine";
Word raTemp1;
double raTemp2;
double realTemp1D;
double realTemp2D;
double realTemp3D;
float realTemp1S;
float realTemp2S;
float realTemp3S;
Word spill[16];
Word stackTopTemp;
Word statusTemp;
Word switchTemp;
Word threadTemp;

#ifndef DEBUG_X86CODEGEN
#define DEBUG_X86CODEGEN FALSE
#endif

#if (defined (__CYGWIN__) || defined (__MSVCRT__))
#define ReturnToC "_Thread_returnToC"
#elif (defined (__FreeBSD__) || defined (__linux__) || defined (__NetBSD__) || defined (__OpenBSD__) || defined (__sun__))
#define ReturnToC "Thread_returnToC"
#else
#error ReturnToC not defined
#endif

#define Main(al, cs, mg, mfs, mmc, pk, ps, ml, reserveEsp)		\
void MLton_jumpToSML (pointer jump) {					\
	Word lc_stackP;							\
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
	s->canHandle += 3;						\
	/* Return to the C Handler thread. */				\
	GC_switchToThread (s, s->callFromCHandler, 0);			\
	jump = *(pointer*)(s->stackTop - WORD_SIZE);			\
	MLton_jumpToSML(jump);						\
	GC_switchToThread (s, s->savedThread, 0);      			\
	s->savedThread = BOGUS_THREAD;					\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "MLton_callFromC() done\n");		\
	return;								\
}									\
int main (int argc, char **argv) {					\
	pointer jump;  							\
	extern pointer ml;						\
	gcState.native = TRUE;						\
	Initialize (al, cs, mg, mfs, mmc, pk, ps);			\
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

