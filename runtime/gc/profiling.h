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

/* If profileStack, then there is one struct GC_profileStack for each
 * function.
 */
typedef struct GC_profileStack {
  /* ticks counts ticks while the function was on the stack. */
  uintmax_t ticks;
  /* ticksGC counts ticks in GC while the function was on the stack. */
  uintmax_t ticksGC; 
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

typedef uint32_t GC_profileMasterIndex;

/* GC_profileData is used for both time and allocation profiling.
 * In the comments below, "ticks" mean clock ticks with time profiling and
 * bytes allocated with allocation profiling.
 *
 * All of the arrays in GC_profileData are of length sourcesLength + sourceNamesLength.
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
  bool isOn;
  GC_profileKind kind;
  bool stack;
};

void enterSourceForProfiling (GC_state s, GC_profileMasterIndex i);
void enterForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex);
void enterFrameForProfiling (GC_state s, GC_frameIndex i);
void GC_profileEnter (GC_state s);

void leaveSourceForProfiling (GC_state s, GC_profileMasterIndex i);
void leaveForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex);
void leaveFrameForProfiling (GC_state s, GC_frameIndex i);
void GC_profileLeave (GC_state s);

void GC_profileInc (GC_state s, size_t amount);
void GC_profileAllocInc (GC_state s, size_t amount);

void initProfiling (GC_state s);

