/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef uint32_t GC_sourceNameIndex;
#define PRISNI PRIu32
#define FMTSNI "%"PRISNI

typedef uint32_t GC_sourceSeqIndex;
#define PRISSI PRIu32
#define FMTSSI "%"PRISSI

#define UNKNOWN_SOURCE_SEQ_INDEX  0
#define GC_SOURCE_SEQ_INDEX       1

typedef uint32_t GC_sourceIndex;
#define PRISI PRIu32
#define FMTSI "%"PRISI

#define UNKNOWN_SOURCE_INDEX  0
#define GC_SOURCE_INDEX       1

typedef struct GC_source {
  GC_sourceNameIndex sourceNameIndex;
  GC_sourceSeqIndex successorSourceSeqIndex;
} *GC_source;

typedef struct GC_profileLabelInfo {
  code_pointer profileLabel;
  GC_sourceSeqIndex sourceSeqIndex;
} *GC_profileLabelInfo;

typedef uint32_t GC_profileLabelInfoIndex;
#define PRISLI PRIu32
#define FMTSLI "%"PRISLI

struct GC_sourceMaps {
  volatile GC_sourceSeqIndex curSourceSeqIndex;
  /* profileLabelInfos is an array of cardinality profileLabelInfosLength;
   * the collection of profile labels embedded in output program
   * paired with an index into sourceSeqs of the sequence of source
   * names corresponding to the code pointer; only used with
   * ProfileTimeLabel.
   */
  struct GC_profileLabelInfo *profileLabelInfos;
  uint32_t profileLabelInfosLength;
  /* sourceNames is an array of cardinality sourceNamesLength;
   * the collection of source names from the program.
   */
  char **sourceNames;
  uint32_t sourceNamesLength;
  /* sourceSeqs is an array of cardinality sourceSeqsLength;
   * each entry describes a sequence of source names as a length
   * followed by the sequence of indices into sources.
   */
  uint32_t **sourceSeqs;
  uint32_t sourceSeqsLength;
  /* sources is an array of cardinality sourcesLength;
   * each entry describes a source name and successor sources as
   * the pair of an index into sourceNames and an index into
   * sourceSeqs.
   */
  struct GC_source *sources;
  uint32_t sourcesLength;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_sourceSeqIndex getCachedStackTopFrameSourceSeqIndex (GC_state s);

static inline char* getSourceName (GC_state s, GC_sourceIndex i);

#if HAS_TIME_PROFILING
static inline int compareProfileLabelInfos (const void *v1, const void *v2);
static void sortProfileLabelInfos (GC_state s);
static void compressProfileLabelInfos (GC_state s);
static void initProfileLabelInfos (GC_state s);
#endif

static void showSources (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE char* GC_sourceName (GC_state s, GC_sourceIndex i);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
