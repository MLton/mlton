#ifndef _CODEGEN_H_
#define _CODEGEN_H_

#define BeginIntInfs static struct GC_intInfInit intInfInits[] = {
#define IntInf(g, n) { g, n },
#define EndIntInfs { 0, NULL }};

#define BeginStrings static struct GC_stringInit stringInits[] = {
#define String(g, s, l) { g, s, l },
#define EndStrings { 0, NULL, 0 }};

#define BeginReals static void real_Init() {
#define Real(c, f) globaldouble[c] = f;
#define EndReals }

#define Globals(c, d, i, p, u, nr)					\
	/* gcState can't be static because stuff in mlton-lib.c refers to it */	\
	struct GC_state gcState;						\
	char globaluchar[c];						\
	double globaldouble[d];						\
	int globalint[i];						\
	pointer globalpointer[p];					\
        uint globaluint[u];						\
	pointer globalpointerNonRoot[nr];				\
	void saveGlobals (int fd) {					\
		swrite (fd, globaluchar, sizeof(char) * c);		\
		swrite (fd, globaldouble, sizeof(double) * d);		\
		swrite (fd, globalint, sizeof(int) * i);		\
		swrite (fd, globalpointer, sizeof(pointer) * p); 	\
		swrite (fd, globaluint, sizeof(uint) * u);		\
	}								\
	static void loadGlobals (FILE *file) {				\
		sfread (globaluchar, sizeof(char), c, file);		\
		sfread (globaldouble, sizeof(double), d, file);		\
		sfread (globalint, sizeof(int), i, file);		\
		sfread (globalpointer, sizeof(pointer), p, file);	\
		sfread (globaluint, sizeof(uint), u, file);		\
	}

#define Initialize(cs, mmc, mfs, mg, pa)				\
	gcState.cardSizeLog2 = cs;					\
	gcState.frameLayouts = frameLayouts;				\
	gcState.globals = globalpointer;				\
	gcState.intInfInits = intInfInits;				\
	gcState.loadGlobals = loadGlobals;				\
	gcState.magic = mg;						\
	gcState.maxFrameSize = mfs;					\
	gcState.mutatorMarksCards = mmc;				\
	gcState.numFrameLayouts = cardof(frameLayouts);			\
	gcState.numGlobals = cardof(globalpointer);			\
	gcState.numObjectTypes = (uint)cardof(objectTypes);		\
	gcState.objectTypes = objectTypes;				\
	gcState.profileAllocIsOn = pa;					\
	gcState.profileLabels = profileLabels;				\
	gcState.profileLabelsSize = cardof(profileLabels);		\
	gcState.profileSources = profileSources;			\
	gcState.profileSourcesSize = cardof(profileSources);		\
	gcState.profileFrameSources = profileFrameSources;		\
	gcState.profileFrameSourcesSize = cardof(profileFrameSources);	\
	gcState.profileSourceSeqs = profileSourceSeqs;			\
	gcState.profileSourceSeqsSize = cardof(profileSourceSeqs);	\
	gcState.saveGlobals = saveGlobals;				\
	gcState.stringInits = stringInits;				\
	MLton_init (argc, argv, &gcState);				\

#endif /* #ifndef _CODEGEN_H_ */
