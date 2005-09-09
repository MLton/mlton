/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

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
