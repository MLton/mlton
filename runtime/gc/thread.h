/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Thread objects are normal objects with the following layout:
 * 
 * header ::
 * padding ::
 * bytesNeeded (size_t) ::
 * exnStack (size_t) ::
 * stack (object-pointer)
 *
 * There may be zero or more bytes of padding for alignment purposes.
 *
 * The bytesNeeded size_t is the number of bytes needed when returning
 * to this thread.
 *
 * The exnStack size_t is an offset added to stackBottom that
 * specifies the top of the exnStack.
 *
 * The final component is the stack object-pointer.
 *
 * Note that the order of the fields is important.  The non-objptr
 * fields must be first, because a thread object must appear to be a
 * normal object.
 */
typedef struct GC_thread {
  size_t bytesNeeded;
  size_t exnStack;
  objptr stack;
} __attribute__ ((packed)) *GC_thread;

COMPILE_TIME_ASSERT(GC_thread__packed,
                    sizeof(struct GC_thread) ==
                    sizeof(size_t)
                    + sizeof(size_t)
                    + sizeof(objptr));

#define BOGUS_EXN_STACK ((size_t)(-1))

#else

struct GC_thread;
typedef struct GC_thread *GC_thread;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void displayThread (GC_state s, GC_thread thread, FILE *stream);
static inline size_t sizeofThread (GC_state s);
static inline size_t offsetofThread (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
