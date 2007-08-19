/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef enum {
  PROFILE_ALLOC,
  PROFILE_COUNT,
  PROFILE_NONE,
  PROFILE_TIME_FIELD,
  PROFILE_TIME_LABEL
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

#else

struct GC_profileData;
typedef struct GC_profileData *GC_profileData;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_profileMasterIndex sourceIndexToProfileMasterIndex (GC_state s, GC_sourceIndex i);
static inline GC_sourceNameIndex profileMasterIndexToSourceNameIndex (GC_state s, GC_profileMasterIndex i);
static inline GC_profileStack getProfileStackInfo (GC_state s, GC_profileMasterIndex i);

static inline void addToStackForProfiling (GC_state s, GC_profileMasterIndex i);
static inline void enterSourceForProfiling (GC_state s, GC_profileMasterIndex i);
static inline void enterForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex);
static inline void enterFrameForProfiling (GC_state s, GC_frameIndex i);

static inline void removeFromStackForProfiling (GC_state s, GC_profileMasterIndex i);
static inline void leaveSourceForProfiling (GC_state s, GC_profileMasterIndex i);
static inline void leaveForProfiling (GC_state s, GC_sourceSeqIndex sourceSeqIndex);
static inline void leaveFrameForProfiling (GC_state s, GC_frameIndex i);

static inline void incForProfiling (GC_state s, size_t amount, GC_sourceSeqIndex sourceSeqIndex);

static inline char* profileIndexSourceName (GC_state s, GC_sourceIndex i);

static void writeProfileCount (GC_state s, FILE *f, GC_profileData p, GC_profileMasterIndex i);

GC_profileData profileMalloc (GC_state s);
void profileWrite (GC_state s, GC_profileData p, const char* fileName);
void profileFree (GC_state s, GC_profileData p);

static void setProfTimer (long usec);
static void initProfilingTime (GC_state s);
static void atexitForProfiling (void);
static void initProfiling (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_profileEnter (GC_state s);
void GC_profileLeave (GC_state s);
void GC_profileInc (GC_state s, size_t amount);
void GC_profileAllocInc (GC_state s, size_t amount);

GC_profileData GC_getProfileCurrent (GC_state s);
void GC_setProfileCurrent (GC_state s, GC_profileData p);

GC_profileData GC_profileMalloc (GC_state s);
void GC_profileWrite (GC_state s, GC_profileData p, NullString8_t fileName);
void GC_profileFree (GC_state s, GC_profileData p);

void GC_profileDone (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

void GC_handleSigProf (code_pointer pc);

