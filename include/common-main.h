/* Copyright (C) 2014,2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _COMMON_MAIN_H_
#define _COMMON_MAIN_H_

#include "mlton-main.h"

#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_BASIS
#include "platform.h"

/* The label must be declared as weak because gcc's optimizer may prove that
 * the code that declares the label is dead and hence eliminate the declaration.
 */
#define DeclareProfileLabel(l)                  \
        extern char l __attribute__ ((weak))

#define LoadArray(a, f) do { if (fread (a, sizeof(*a), cardof(a), f) != cardof(a)) return -1; } while (0)
#define SaveArray(a, f) do { if (fwrite(a, sizeof(*a), cardof(a), f) != cardof(a)) return -1; } while (0)

#define Initialize(s, al, mg, mfs, mmc, pk, ps)                         \
        s->alignment = al;                                              \
        s->atMLtons = atMLtons;                                         \
        s->atMLtonsLength = cardof(atMLtons);                           \
        s->frameInfos = frameInfos;                                     \
        s->frameInfosLength = cardof(frameInfos);                       \
        s->globals = (objptr*)globalObjptr;                             \
        s->globalsLength = cardof(globalObjptr);                        \
        s->loadGlobals = loadGlobals;                                   \
        s->magic = mg;                                                  \
        s->maxFrameSize = mfs;                                          \
        s->mutatorMarksCards = mmc;                                     \
        s->objectTypes = objectTypes;                                   \
        s->objectTypesLength = cardof(objectTypes);                     \
        s->returnAddressToFrameIndex = returnAddressToFrameIndex;       \
        s->saveGlobals = saveGlobals;                                   \
        s->sourceMaps.profileLabelInfos = profileLabelInfos;            \
        s->sourceMaps.profileLabelInfosLength = cardof(profileLabelInfos); \
        s->sourceMaps.sourceNames = sourceNames;                        \
        s->sourceMaps.sourceNamesLength = cardof(sourceNames);          \
        s->sourceMaps.sourceSeqs = sourceSeqs;                          \
        s->sourceMaps.sourceSeqsLength = cardof(sourceSeqs);            \
        s->sourceMaps.sources = sources;                                \
        s->sourceMaps.sourcesLength = cardof(sources);                  \
        s->staticHeaps.dynamic.start = (pointer)&staticHeapD;           \
        s->staticHeaps.dynamic.size = (pointer)&staticHeapD.end - (pointer)&staticHeapD; \
        s->staticHeaps.immutable.start = (pointer)&staticHeapI;         \
        s->staticHeaps.immutable.size = (pointer)&staticHeapI.end - (pointer)&staticHeapI; \
        s->staticHeaps.mutable.start = (pointer)&staticHeapM;           \
        s->staticHeaps.mutable.size = (pointer)&staticHeapM.end - (pointer)&staticHeapM; \
        s->staticHeaps.root.start = (pointer)&staticHeapR;              \
        s->staticHeaps.root.size = (pointer)&staticHeapR.end - (pointer)&staticHeapR; \
        s->profiling.kind = pk;                                         \
        s->profiling.stack = ps;                                        \
        MLton_init (argc, argv, s);

#define LIB_PASTE(x,y) x ## y
#define LIB_OPEN(x) LIB_PASTE(x, _open)
#define LIB_CLOSE(x) LIB_PASTE(x, _close)

#endif /* #ifndef _COMMON_MAIN_H_ */
