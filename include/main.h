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

#define LoadArray(a, f) sfread (a, sizeof(*a), cardof(a), f)
#define SaveArray(a, fd) swrite (fd, a, sizeof(*a) * cardof(a))

#define Initialize(al, cs, mg, mfs, mlw, mmc, ps)			\
	gcState.alignment = al;						\
	gcState.cardSizeLog2 = cs;					\
	gcState.frameLayouts = frameLayouts;				\
	gcState.frameLayoutsSize = cardof(frameLayouts); 		\
	gcState.frameSources = frameSources;				\
	gcState.frameSourcesSize = cardof(frameSources);		\
	gcState.globals = globalPointer;				\
	gcState.globalsSize = cardof(globalPointer);			\
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
	gcState.saveGlobals = saveGlobals;				\
	gcState.sourceLabels = sourceLabels;				\
	gcState.sourceLabelsSize = cardof(sourceLabels);		\
	gcState.sourceNames = sourceNames;				\
	gcState.sourceNamesSize = cardof(sourceNames);			\
	gcState.sourceSeqs = sourceSeqs;				\
	gcState.sourceSeqsSize = cardof(sourceSeqs);			\
	gcState.sources = sources;					\
	gcState.sourcesSize = cardof(sources);				\
	gcState.stringInits = stringInits;				\
	gcState.stringInitsSize = cardof(stringInits);			\
	MLton_init (argc, argv, &gcState);				\

void MLton_callFromC ();

#endif /* #ifndef _MAIN_H_ */
