/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef enum {
  PROFILE_ALLOC,
  PROFILE_COUNT,
  PROFILE_NONE,
  PROFILE_TIME,
} GC_profileKind;

typedef struct GC_source {
  uint32_t nameIndex;
  uint32_t successorsIndex;
} *GC_source;

typedef struct GC_sourceLabel {
  pointer label;
  uint32_t sourceSeqsIndex;
} *GC_sourceLabel;

/* If profileStack, then there is one struct GC_profileStack for each
 * function.
 */
typedef struct GC_profileStack {
  /* ticks counts ticks while the function was on the stack. */
  uintmax_t ticks;
  /* ticksInGC counts ticks in GC while the function was on the stack. */
  uintmax_t ticksInGC; 
  /* lastTotal is the value of total when the oldest occurrence of f
   * on the stack was pushed, i.e., the most recent time that
   * numTimesOnStack changed from 0 to 1.  lastTotal is used to
   * compute the amount to attribute to f when the oldest occurrence
   * is finally popped.
   */
  uintmax_t lastTotal;
  /* lastTotalGC is like lastTotal, but for GC ticks. */
  uintmax_t lastTotalGC;
  /* numOccurrences is the number of times this function is on the
   * stack.
   */
  uintmax_t numOccurrences;
} *GC_profileStack;

/* GC_profileData is used for both time and allocation profiling.
 * In the comments below, "ticks" mean clock ticks with time profiling and
 * bytes allocated with allocation profiling.
 *
 * All of the arrays in GC_profileData are of length sourcesSize + sourceNamesSize.
 * The first sourceLength entries are for handling the duplicate copies of 
 * functions, and the next sourceNamesLength entries are for the master versions.
 */
typedef struct GC_profileData {
  /* countTop is an array that counts for each function the number of
   * ticks that occurred while the function was on top of the stack.
   */
  uintmax_t *countTop;
  /* stack is an array that gives stack info for each function.  
   * It is only used if profileStack.
   */
  struct GC_profileStack *stack;
  /* The total number of mutator ticks. */
  uintmax_t total;
  /* The total number of GC ticks. */
  uintmax_t totalGC;
} *GC_profileData;

struct GC_profiling {
  GC_profileData data;
  /* frameSources is an array of cardinality frameLayoutsLength that
   * for each stack frame, gives an index into sourceSeqs of the
   * sequence of source functions corresponding to the frame.
   */
  uint32_t *frameSources;
  uint32_t frameSourcesLength;
  bool isOn;
  GC_profileKind kind;
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
  bool stack;
  pointer textEnd;
  /* An array of indices, one entry for each address in the text
   * segment, giving and index into sourceSeqs.
   */
  uint32_t *textSources;
  pointer textStart;
};

static void showProf (GC_state s);
void initProfiling (GC_state s);
static void enterFrame (GC_state s, uint32_t i);


void GC_profileAllocInc (GC_state s, size_t bytes);

void GC_profileEnter (GC_state s);

void GC_profileLeave (GC_state s);
