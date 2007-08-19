/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Stack objects have the following layout:
 * 
 * header ::
 * markTop (native-pointer) ::
 * markIndex (word32) ::
 * reserved ::
 * used ::
 * ... reserved bytes ...
 *
 * The markTop and markIndex are used by the mark-compact GC.  The
 * reserved size gives the number of bytes for the stack (before the
 * next ML object).  The used size gives the number of bytes currently
 * used by the stack.  The sequence of reserved bytes correspond to ML
 * stack frames, which will be discussed in more detail in "frame.h".
*/
typedef struct GC_stack {       
  /* markTop and markIndex are only used during marking.  They record
   * the current pointer in the stack that is being followed.  markTop
   * points to the top of the stack frame containing the pointer and
   * markIndex is the index in that frame's frameOffsets of the pointer
   * slot.  So, when the GC pointer reversal gets back to the stack,
   * it can continue with the next pointer (either in the current
   * frame or the next frame).
   */
  pointer markTop;
  uint32_t markIndex;
  /* reserved is the number of bytes reserved for stack, 
   * i.e. its maximum size.
   */
  size_t reserved;
  /* used is the number of bytes used by the stack.  
   * Stacks with used == reserved are continuations.
   */
  size_t used;      
  /* The next address is the bottom of the stack, and the following
   * reserved bytes hold space for the stack.
   */
} *GC_stack;

#define GC_STACK_HEADER_SIZE GC_HEADER_SIZE

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayStack (GC_state s, GC_stack stack, FILE *stream);

static inline bool isStackEmpty (GC_stack stack);
#if ASSERT
static inline bool isStackReservedAligned (GC_state s, size_t reserved);
#endif

static inline size_t sizeofStackSlop (GC_state s);
static inline size_t sizeofStackInitial (GC_state s);

static inline pointer getStackBottom (GC_state s, GC_stack stack);
static inline pointer getStackTop (GC_state s, GC_stack stack);
static inline pointer getStackLimitPlusSlop (GC_state s, GC_stack stack);
static inline pointer getStackLimit (GC_state s, GC_stack stack);
static inline GC_frameIndex getCachedStackTopFrameIndex (GC_state s);
static inline GC_frameLayout getCachedStackTopFrameLayout (GC_state s);
static inline GC_frameIndex getStackTopFrameIndex (GC_state s, GC_stack stack);
static inline GC_frameLayout getStackTopFrameLayout (GC_state s, GC_stack stack);
static inline uint16_t getStackTopFrameSize (GC_state s, GC_stack stack);

static inline size_t sizeofStackMinimumReserved (GC_state s, GC_stack stack);
static inline size_t alignStackReserved (GC_state s, size_t reserved);
static inline size_t sizeofStackWithHeaderAligned (GC_state s, size_t reserved);
static inline size_t sizeofStackGrow (GC_state s, GC_stack stack);

static inline void copyStack (GC_state s, GC_stack from, GC_stack to);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
