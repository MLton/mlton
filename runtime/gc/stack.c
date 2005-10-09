/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool stackIsEmpty (GC_stack stack) {
  return 0 == stack->used;
}

/* stackSlop returns the amount of "slop" space needed between the top
 * of the stack and the end of the stack space.
 */
static inline size_t stackSlop (GC_state s) {
  return (size_t)(2 * s->maxFrameSize);
}

static inline size_t initialStackSize (GC_state s) {
  return stackSlop (s);
}

static inline size_t stackNumBytes (GC_state s, size_t size) {
  size_t res;
  
  res = align (GC_STACK_HEADER_SIZE + sizeof (struct GC_stack) + size,
               s->alignment);
  if (DEBUG_STACKS)
    fprintf (stderr, "%zu = stackNumBytes (%zu)\n", res, size);
  return res;
}

static inline pointer stackBottom (GC_state s, GC_stack stack) {
  pointer res;
  
  res = ((pointer)stack) + sizeof (struct GC_stack);
  assert (isAligned ((uintptr_t)res, s->alignment));
  return res;
}

/* Pointer to the topmost word in use on the stack. */
static inline pointer stackTop (GC_state s, GC_stack stack) {
  return stackBottom (s, stack) + stack->used;
}

/* Pointer to the end of stack. */
static inline pointer stackLimitPlusSlop (GC_state s, GC_stack stack) {
  return stackBottom (s, stack) + stack->reserved;
}

/* The maximum value stackTop may take on. */
static inline pointer stackLimit (GC_state s, GC_stack stack) {
  return stackLimitPlusSlop (s, stack) - stackSlop (s);
}

static inline uint32_t topFrameIndex (GC_state s, GC_stack stack) {
  uint32_t res;
  
  res = 
    getFrameIndexFromReturnAddress 
    (s, *(GC_returnAddress*)(stackTop (s, stack) - GC_RETURNADDRESS_SIZE));
  if (DEBUG_PROFILE)
    fprintf (stderr, "topFrameIndex = %"PRIu32"\n", res);
  return res;
}

static inline GC_frameLayout * topFrameLayout (GC_state s, GC_stack stack) {
  GC_frameLayout *layout;

  layout = getFrameLayoutFromFrameIndex (s, topFrameIndex (s, stack));
  return layout;
}

static inline uint16_t topFrameSize (GC_state s, GC_stack stack) {
  GC_frameLayout *layout;
  
  assert (not (stackIsEmpty (stack)));
  layout = topFrameLayout (s, stack);
  return layout->size;
}

static inline size_t stackReserved (GC_state s, size_t r) {
  size_t res;
  
  res = pad (s, r, GC_STACK_HEADER_SIZE + sizeof (struct GC_stack));
  if (DEBUG_STACKS)
    fprintf (stderr, "%zu = stackReserved (%zu)\n", res, r);
  return res;
}

static inline size_t stackNeedsReserved (GC_state s, GC_stack stack) {
  return stack->used + stackSlop (s) - topFrameSize(s, stack);
}

static inline void stackCopy (GC_state s, GC_stack from, GC_stack to) {
  pointer fromBottom, toBottom;

  fromBottom = stackBottom (s, from);
  toBottom = stackBottom (s, to);
  assert (from->used <= to->reserved);
  to->used = from->used;
  if (DEBUG_STACKS)
    fprintf (stderr, "stackCopy from "FMTPTR" to "FMTPTR" of length %zd\n",
             (uintptr_t) fromBottom, 
             (uintptr_t) toBottom,
             from->used);
  memcpy (fromBottom, toBottom, from->used);
}

void displayStack (__attribute__ ((unused)) GC_state s,
                   GC_stack stack, 
                   FILE *stream) {
  fprintf(stream,
          "\t\treserved = %zu\n"
          "\t\tused = %zu\n",
          stack->reserved,
          stack->used);
}
