#ifndef _X86CODEGEN_H_
#define _X86CODEGEN_H_

#include "codegen.h"

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


#define Locals(c, d, i, p, u)						\
	char localuchar[c];						\
	double localdouble[d];				       		\
	int localint[i];						\
	pointer localpointer[p];					\
	uint localuint[u]

#define Main(cs, mg, mfs, mlw, mmc, ps, ml, reserveEsp)			\
void MLton_callFromC () {						\
	pointer jump;							\
	GC_state s;							\
									\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "MLton_callFromC() starting\n");	\
	s = &gcState;							\
	s->savedThread = s->currentThread;				\
	/* Return to the C Handler thread. */				\
	GC_switchToThread (s, s->callFromCHandler);			\
	jump = *(pointer*)(s->stackTop - WORD_SIZE);			\
	if (reserveEsp)							\
		__asm__ __volatile__					\
		("pusha;movl %%esp,%0;movl %1,%%eax;movl %2,%%ebp;movl %3,%%edi;jmp *%%eax;.global Thread_returnToC;Thread_returnToC:;movl %0,%%esp;popa" \
		: "=m" (c_stackP)					\
		: "m" (jump), "m" (gcState.stackTop), "m" (gcState.frontier) \
		);							\
	else								\
		__asm__ __volatile__ 					\
		("pusha;movl %%esp,%0;movl %1,%%eax;movl %2,%%ebp;movl %3,%%esp;jmp *%%eax;.global Thread_returnToC;Thread_returnToC:;movl %0,%%esp;popa" \
		: "=m" (c_stackP) 					\
 		: "m" (jump), "m" (gcState.stackTop), "m" (gcState.frontier) \
		); \
	GC_switchToThread (s, s->savedThread);				\
	s->savedThread = BOGUS_THREAD;					\
	if (DEBUG_X86CODEGEN)						\
		fprintf (stderr, "Thread_returnToC done");		\
	return;								\
}									\
int main (int argc, char **argv) {					\
	pointer jump;  							\
	extern pointer ml;						\
	gcState.native = TRUE;						\
	Initialize(cs, mg, mfs, mlw, mmc, ps);				\
	if (gcState.isOriginal) {					\
		real_Init();						\
		jump = (pointer)&ml;   					\
	} else {       							\
		jump = *(pointer*)(gcState.stackTop - WORD_SIZE); 	\
	}								\
	if (reserveEsp)							\
		__asm__ __volatile__					\
		("movl %%esp,%0;movl %1,%%eax;movl %2,%%ebp;movl %3,%%edi;jmp *%%eax" \
		: "=m" (c_stackP)					\
		: "m" (jump), "m" (gcState.stackTop), "m" (gcState.frontier) \
		);							\
	else								\
		__asm__ __volatile__ 					\
		("movl %%esp,%0;movl %1,%%eax;movl %2,%%ebp;movl %3,%%esp;jmp *%%eax" \
		: "=m" (c_stackP) 					\
 		: "m" (jump), "m" (gcState.stackTop), "m" (gcState.frontier) \
		);							\
	return 1;							\
}

#endif /* #ifndef _X86CODEGEN_H_ */
