/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MAIN_H_
#define _MAIN_H_

#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_BASIS
#include "platform.h"

typedef Pointer CPointer;
typedef Pointer Objptr;

/* The label must be declared as weak because gcc's optimizer may prove that
 * the code that declares the label is dead and hence eliminate the declaration.
 */
#define DeclareProfileLabel(l)                  \
        extern char l __attribute__ ((weak))

#define BeginIntInfs static struct GC_intInfInit intInfInits[] = {
#define IntInf(g, n) { g, n },
#define EndIntInfs };

#define BeginVectors static struct GC_vectorInit vectorInits[] = {
#define Vector(a, b, c, d) { a, b, c, d },
#define EndVectors };

#define LoadArray(a, f) if (fread (a, sizeof(*a), cardof(a), f) != cardof(a)) return -1;
#define SaveArray(a, f) if (fwrite(a, sizeof(*a), cardof(a), f) != cardof(a)) return -1;

Pointer gcStateAddress;

#define Initialize(al, mg, mfs, mmc, pk, ps)                            \
        gcStateAddress = &gcState;                                      \
        gcState.alignment = al;                                         \
        gcState.atMLtons = atMLtons;                                    \
        gcState.atMLtonsLength = cardof(atMLtons);                      \
        gcState.frameLayouts = frameLayouts;                            \
        gcState.frameLayoutsLength = cardof(frameLayouts);              \
        gcState.globals = globalObjptr;                                 \
        gcState.globalsLength = cardof(globalObjptr);                   \
        gcState.intInfInits = intInfInits;                              \
        gcState.intInfInitsLength = cardof(intInfInits);                \
        gcState.loadGlobals = loadGlobals;                              \
        gcState.magic = mg;                                             \
        gcState.maxFrameSize = mfs;                                     \
        gcState.mutatorMarksCards = mmc;                                \
        gcState.objectTypes = objectTypes;                              \
        gcState.objectTypesLength = cardof(objectTypes);                \
        gcState.returnAddressToFrameIndex = returnAddressToFrameIndex;  \
        gcState.saveGlobals = saveGlobals;                              \
        gcState.vectorInits = vectorInits;                              \
        gcState.vectorInitsLength = cardof(vectorInits);                \
        gcState.sourceMaps.frameSources = frameSources;                 \
        gcState.sourceMaps.frameSourcesLength = cardof(frameSources);   \
        gcState.sourceMaps.sourceLabels = sourceLabels;                 \
        gcState.sourceMaps.sourceLabelsLength = cardof(sourceLabels);   \
        gcState.sourceMaps.sourceNames = sourceNames;                   \
        gcState.sourceMaps.sourceNamesLength = cardof(sourceNames);     \
        gcState.sourceMaps.sourceSeqs = sourceSeqs;                     \
        gcState.sourceMaps.sourceSeqsLength = cardof(sourceSeqs);       \
        gcState.sourceMaps.sources = sources;                           \
        gcState.sourceMaps.sourcesLength = cardof(sources);             \
        gcState.profiling.kind = pk;                                    \
        gcState.profiling.stack = ps;                                   \
        MLton_init (argc, argv, &gcState);                              \

void MLton_callFromC ();

#endif /* #ifndef _MAIN_H_ */
