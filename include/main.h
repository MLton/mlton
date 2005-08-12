/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MAIN_H_
#define _MAIN_H_

#include "platform.h"

/* The label must be declared as weak because gcc's optimizer may prove that
 * the code that declares the label is dead and hence eliminate the declaration.
 */
#define DeclareProfileLabel(l)			\
	extern char l __attribute__ ((weak))

#define BeginIntInfs static struct GC_intInfInit intInfInits[] = {
#define IntInf(g, n) { g, n },
#define EndIntInfs };

#define BeginVectors static struct GC_vectorInit vectorInits[] = {
#define Vector(a, b, c, d) { a, b, c, d },
#define EndVectors };

#define LoadArray(a, f) sfread (a, sizeof(*a), cardof(a), f)
#define SaveArray(a, fd) swrite (fd, a, sizeof(*a) * cardof(a))

Pointer gcStateAddress;

#define Initialize(al, mg, mfs, mmc, pk, ps)				\
	gcStateAddress = &gcState;					\
	gcState.alignment = al;						\
	gcState.atMLtons = atMLtons;					\
	gcState.atMLtonsSize = cardof(atMLtons);		       	\
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
	gcState.mutatorMarksCards = mmc;				\
	gcState.objectTypes = objectTypes;				\
	gcState.objectTypesSize = cardof(objectTypes);			\
	gcState.profileKind = pk;					\
	gcState.profileStack = ps;					\
	gcState.returnAddressToFrameIndex = returnAddressToFrameIndex;	\
	gcState.saveGlobals = saveGlobals;				\
	gcState.sourceLabels = sourceLabels;				\
	gcState.sourceLabelsSize = cardof(sourceLabels);		\
	gcState.sourceNames = sourceNames;				\
	gcState.sourceNamesSize = cardof(sourceNames);			\
	gcState.sourceSeqs = sourceSeqs;				\
	gcState.sourceSeqsSize = cardof(sourceSeqs);			\
	gcState.sources = sources;					\
	gcState.sourcesSize = cardof(sources);				\
	gcState.vectorInits = vectorInits;				\
	gcState.vectorInitsSize = cardof(vectorInits);			\
	MLton_init (argc, argv, &gcState);				\

void MLton_callFromC ();

#endif /* #ifndef _MAIN_H_ */
