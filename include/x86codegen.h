#ifndef _X86CODEGEN_H_
#define _X86CODEGEN_H_

#define Globals(c, d, i, p, u, nr)					\
	word raTemp1;							\
	double raTemp2;							\
	word spill[16];							\
	word indexTemp;							\
	word checkTemp;							\
	word divTemp;							\
	struct GC_state gcState;					\
	word c_stackP;							\
	char cReturnTempB;						\
	word cReturnTempL;						\
	double cReturnTempD;						\
	word switchTemp;						\
	word intInfTemp;						\
	word threadTemp;						\
	word statusTemp;						\
	word fileTemp;							\
	word applyFFTemp;						\
	double realTemp1;						\
	double realTemp2;						\
        double realTemp3;						\
	word fpswTemp;							\
	char MLton_bug_msg[] = "cps machine";				\
	char globaluchar[c];						\
	double globaldouble[d];						\
	int globalint[i];						\
	pointer globalpointer[p];					\
        uint globaluint[u];						\
	pointer globalpointerNonRoot[nr];				\
	void saveGlobals(int fd) {					\
		swrite(fd, globaluchar, sizeof(char) * c);		\
		swrite(fd, globaldouble, sizeof(double) * d);		\
		swrite(fd, globalint, sizeof(int) * i);			\
		swrite(fd, globalpointer, sizeof(pointer) * p);		\
		swrite(fd, globaluint, sizeof(uint) * u);		\
	}								\
	static void loadGlobals(FILE *file) {				\
		sfread(globaluchar, sizeof(char), c, file);		\
		sfread(globaldouble, sizeof(double), d, file);		\
		sfread(globalint, sizeof(int), i, file);		\
		sfread(globalpointer, sizeof(pointer), p, file);	\
		sfread(globaluint, sizeof(uint), u, file);		\
	}

#define Locals(c, d, i, p, u)						\
	char localuchar[c];						\
	double localdouble[d];				       		\
	int localint[i];						\
	pointer localpointer[p];					\
	uint localuint[u]

#define BeginIntInfs static struct GC_intInfInit intInfInits[] = {
#define IntInf(g, n) { g, n },
#define EndIntInfs { 0, NULL }};

#define BeginStrings static struct GC_stringInit stringInits[] = {
#define String(g, s, l) { g, s, l },
#define EndStrings { 0, NULL, 0 }};

#define BeginFloats static void float_Init() {
#define Float(c, f) globaldouble[c] = f;
#define EndFloats }

#define Main(cs, mmc, mfs, mfi, mot, mg, ml, reserveEsp)		\
extern pointer ml;							\
int main (int argc, char **argv) {					\
	pointer jump;  							\
	gcState.cardSizeLog2 = cs;					\
	gcState.frameLayouts = frameLayouts;				\
	gcState.globals = globalpointer;				\
	gcState.intInfInits = intInfInits;				\
	gcState.loadGlobals = &loadGlobals;				\
	gcState.magic = mg;						\
	gcState.maxFrameIndex = mfi;					\
	gcState.maxFrameSize = mfs;					\
	gcState.maxObjectTypeIndex = mot;				\
	gcState.mutatorMarksCards = mmc;				\
	gcState.native = TRUE;       					\
	gcState.numGlobals = cardof(globalpointer);			\
	gcState.objectTypes = objectTypes;				\
	gcState.saveGlobals = &saveGlobals;				\
	gcState.stringInits = stringInits;				\
	MLton_init (argc, argv, &gcState);				\
	if (gcState.isOriginal) {					\
		float_Init();						\
		jump = (pointer)&ml;   					\
	} else {       							\
		jump = *(pointer*)(gcState.stackTop - WORD_SIZE); 	\
	}								\
	if (reserveEsp)							\
		__asm__ __volatile__					\
		("movl %%esp,%0;movl %1,%%eax;movl %2,%%ebp;movl %3,%%edi;jmp *%%eax" \
		: "=m" (c_stackP)					\
		: "m" (jump), "m" (gcState.stackTop), "m" (gcState.frontier) \
		: "%ebp", "%edi");					\
	else								\
		__asm__ __volatile__ 					\
		("movl %%esp,%0;movl %1,%%eax;movl %2,%%ebp;movl %3,%%esp;jmp *%%eax" \
		: "=m" (c_stackP) 					\
 		: "m" (jump), "m" (gcState.stackTop), "m" (gcState.frontier) \
		: "%ebp", "%esp");					\
	return 1;							\
}

#endif /* #ifndef _X86CODEGEN_H_ */
