#ifndef _MAIN_H_
#define _MAIN_H_

#include "libmlton.h"

/* The label must be declared as weak because gcc's optimizer may prove that
 * the code that declares the label is dead and hence eliminate the declaration.
 */
#define DeclareProfileLabel(l)			\
	void l() __attribute__ ((weak))

#define BeginIntInfs static struct GC_intInfInit intInfInits[] = {
#define IntInf(g, n) { g, n },
#define EndIntInfs };

#define BeginStrings static struct GC_stringInit stringInits[] = {
#define String(g, s, l) { g, s, l },
#define EndStrings };

#define BeginReals static void real_Init() {
#define Real(c, f) globaldouble[c] = f;
#define EndReals }

#define LoadArray(a, f) sfread (a, sizeof(*a), cardof(a), f)
#define SaveArray(a, fd) swrite (fd, a, sizeof(*a) * cardof(a))

/* gcState can't be static because stuff in mlton-lib.c refers to it */

#define Globals(c, d, i, p, u, nr)			\
	struct GC_state gcState;			\
	char globaluchar[c];				\
	double globaldouble[d];				\
	int globalint[i];				\
	pointer globalpointer[p];			\
        uint globaluint[u];				\
	pointer globalpointerNonRoot[nr];		\
	static void saveGlobals (int fd) {		\
		SaveArray (globaluchar, fd);		\
		SaveArray (globaldouble, fd);		\
		SaveArray (globalint, fd);		\
		SaveArray (globalpointer, fd);		\
		SaveArray (globaluint, fd);		\
	}						\
	static void loadGlobals (FILE *file) {		\
		LoadArray (globaluchar, file);		\
		LoadArray (globaldouble, file);		\
		LoadArray (globalint, file);		\
		LoadArray (globalpointer, file);	\
		LoadArray (globaluint, file);		\
	}

#define Initialize(al, cs, mg, mfs, mlw, mmc, ps)			\
	gcState.alignment = al;						\
	gcState.cardSizeLog2 = cs;					\
	gcState.frameLayouts = frameLayouts;				\
	gcState.frameLayoutsSize = cardof(frameLayouts); 		\
	gcState.frameSources = frameSources;				\
	gcState.frameSourcesSize = cardof(frameSources);		\
	gcState.globals = globalpointer;				\
	gcState.globalsSize = cardof(globalpointer);			\
	gcState.intInfInits = intInfInits;				\
	gcState.intInfInitsSize = cardof(intInfInits);			\
	gcState.loadGlobals = loadGlobals;				\
	gcState.magic = mg;						\
	gcState.maxFrameSize = mfs;					\
	gcState.mayLoadWorld = mlw;					\
	gcState.mutatorMarksCards = mmc;				\
	gcState.objectTypes = objectTypes;				\
	gcState.objectTypesSize = cardof(objectTypes);			\
	gcState.profileStack = ps;					\
	gcState.sourceLabels = sourceLabels;				\
	gcState.sourceLabelsSize = cardof(sourceLabels);		\
	gcState.saveGlobals = saveGlobals;				\
	gcState.sources = sources;					\
	gcState.sourcesSize = cardof(sources);				\
	gcState.sourceSeqs = sourceSeqs;				\
	gcState.sourceSeqsSize = cardof(sourceSeqs);			\
	gcState.sourceSuccessors = sourceSuccessors;			\
	gcState.stringInits = stringInits;				\
	gcState.stringInitsSize = cardof(stringInits);			\
	MLton_init (argc, argv, &gcState);				\

#endif /* #ifndef _MAIN_H_ */
