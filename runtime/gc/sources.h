/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef uint32_t GC_sourceNameIndex;
#define PRISNI PRIu32
#define FMTSNI "%"PRISNI

typedef uint32_t GC_sourceLabelIndex;
#define PRISLI PRIu32
#define FMTSLI "%"PRISLI

typedef uint32_t GC_sourceIndex;
#define PRISI PRIu32
#define FMTSI "%"PRISI

#define SOURCES_INDEX_UNKNOWN 0
#define SOURCES_INDEX_GC      1

typedef uint32_t GC_sourceSeqIndex;
#define PRISSI PRIu32
#define FMTSSI "%"PRISSI

#define SOURCE_SEQ_UNKNOWN    0
#define SOURCE_SEQ_GC         1

typedef struct GC_source {
  GC_sourceNameIndex sourceNameIndex;
  GC_sourceSeqIndex successorSourceSeqIndex;
} *GC_source;

typedef struct GC_sourceLabel {
  code_pointer label;
  GC_sourceSeqIndex sourceSeqIndex;
} *GC_sourceLabel;

struct GC_sourceMaps {
  volatile GC_sourceSeqIndex curSourceSeqsIndex;
  /* frameSources is an array of cardinality frameLayoutsLength that
   * for each stack frame, gives an index into sourceSeqs of the
   * sequence of source functions corresponding to the frame.
   */
  GC_sourceSeqIndex *frameSources;
  uint32_t frameSourcesLength;
  struct GC_sourceLabel *sourceLabels;
  uint32_t sourceLabelsLength;
  char **sourceNames;
  uint32_t sourceNamesLength;
  /* Each entry in sourceSeqs is a vector, whose first element is a
   * length, and subsequent elements index into sources.
   */
  uint32_t **sourceSeqs;
  uint32_t sourceSeqsLength;
  /* sources is an array of cardinality sourcesLength.  Each entry
   * specifies an index into sourceNames and an index into sourceSeqs,
   * giving the name of the function and the successors, respectively.
   */
  struct GC_source *sources;
  uint32_t sourcesLength;
  code_pointer textEnd;
  /* An array of indices, one entry for each address in the text
   * segment, giving an index into sourceSeqs.
   */
  GC_sourceSeqIndex *textSources;
  code_pointer textStart;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_sourceSeqIndex getCachedStackTopFrameSourceSeqIndex (GC_state s);

static inline char* getSourceName (GC_state s, GC_sourceIndex i);

#if HAS_TIME_PROFILING
static inline int compareSourceLabels (const void *v1, const void *v2);
static void sortSourceLabels (GC_state s);
static void initTextSources (GC_state s);
#endif

static void showSources (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

char* GC_sourceName (GC_state s, GC_sourceIndex i);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
