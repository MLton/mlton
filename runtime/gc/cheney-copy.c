/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                    Cheney Copying Collection                     */
/* ---------------------------------------------------------------- */

/* forward (s, pp) forwards the object pointed to by *pp and updates *pp to 
 * point to the new object. 
 * It also updates the crossMap.
 */
static inline void forward (GC_state s, pointer *pp) {
  pointer p;
  GC_ObjectHeader header;
  GC_ObjectTypeTag tag;

  if (DEBUG_DETAILED)
    fprintf (stderr, 
             "forward  pp = 0x"PRIxPTR"  *pp = 0x"PRIxPTR"\n", 
             pp, *pp);
  assert (isInFromSpace (s, *pp));
  p = *pp;
  header = GC_getHeader (p);
        if (DEBUG_DETAILED and FORWARDED == header)
                fprintf (stderr, "already FORWARDED\n");
        if (header != FORWARDED) { /* forward the object */
                Bool hasIdentity;
                uint headerBytes, objectBytes, size, skip;
                uint numPointers, numNonPointers;

                /* Compute the space taken by the header and object body. */
                SPLIT_HEADER();
                if (NORMAL_TAG == tag) { /* Fixed size object. */
                        headerBytes = GC_NORMAL_HEADER_SIZE;
                        objectBytes = toBytes (numPointers + numNonPointers);
                        skip = 0;
                } else if (ARRAY_TAG == tag) {
                        headerBytes = GC_ARRAY_HEADER_SIZE;
                        objectBytes = arrayNumBytes (s, p, numPointers,
                                                        numNonPointers);
                        skip = 0;
                } else if (WEAK_TAG == tag) {
                        headerBytes = GC_NORMAL_HEADER_SIZE;
                        objectBytes = sizeof (struct GC_weak);
                        skip = 0;
                } else { /* Stack. */
                        GC_stack stack;

                        assert (STACK_TAG == tag);
                        headerBytes = STACK_HEADER_SIZE;
                        stack = (GC_stack)p;

                        if (s->currentThread->stack == stack) {
                                /* Shrink stacks that don't use a lot 
                                 * of their reserved space;
                                 * but don't violate the stack invariant.
                                 */
                                if (stack->used <= stack->reserved / 4) {
                                        uint new = stackReserved (s, max (stack->reserved / 2,
                                                                                stackNeedsReserved (s, stack)));
                                        /* It's possible that new > stack->reserved if
                                         * the stack invariant is violated. In that case, 
                                         * we want to leave the stack alone, because some 
                                         * other part of the gc will grow the stack.  We 
                                         * cannot do any growing here because we may run 
                                         * out of to space.
                                         */
                                        if (new <= stack->reserved) {
                                                stack->reserved = new;
                                                if (DEBUG_STACKS)
                                                        fprintf (stderr, "Shrinking stack to size %s.\n",
                                                                        uintToCommaString (stack->reserved));
                                        }
                                }
                        } else {
                                /* Shrink heap stacks.
                                 */
                                stack->reserved = stackReserved (s, max(s->threadShrinkRatio * stack->reserved, 
                                                                        stack->used));
                                if (DEBUG_STACKS)
                                        fprintf (stderr, "Shrinking stack to size %s.\n",
                                                        uintToCommaString (stack->reserved));
                        }
                        objectBytes = sizeof (struct GC_stack) + stack->used;
                        skip = stack->reserved - stack->used;
                }
                size = headerBytes + objectBytes;
                assert (s->back + size + skip <= s->toLimit);
                /* Copy the object. */
                copy (p - headerBytes, s->back, size);
                /* If the object has a valid weak pointer, link it into the weaks
                 * for update after the copying GC is done.
                 */
                if (WEAK_TAG == tag and 1 == numPointers) {
                        GC_weak w;

                        w = (GC_weak)(s->back + GC_NORMAL_HEADER_SIZE);
                        if (DEBUG_WEAK)
                                fprintf (stderr, "forwarding weak 0x%08x ",
                                                (uint)w);
                        if (GC_isPointer (w->object)
                                and (not s->amInMinorGC
                                        or isInNursery (s, w->object))) {
                                if (DEBUG_WEAK)
                                        fprintf (stderr, "linking\n");
                                w->link = s->weaks;
                                s->weaks = w;
                        } else {
                                if (DEBUG_WEAK)
                                        fprintf (stderr, "not linking\n");
                        }
                }
                /* Store the forwarding pointer in the old object. */
                *(word*)(p - WORD_SIZE) = FORWARDED;
                *(pointer*)p = s->back + headerBytes;
                /* Update the back of the queue. */
                s->back += size + skip;
                assert (isAligned ((uint)s->back + GC_NORMAL_HEADER_SIZE,
                                        s->alignment));
        }
        *pp = *(pointer*)p;
        assert (isInToSpace (s, *pp));
}

static void updateWeaks (GC_state s) {
        GC_weak w;

        for (w = s->weaks; w != NULL; w = w->link) {
                assert ((pointer)BOGUS_POINTER != w->object);

                if (DEBUG_WEAK)
                        fprintf (stderr, "updateWeaks  w = 0x%08x  ", (uint)w);
                if (FORWARDED == GC_getHeader ((pointer)w->object)) {
                        if (DEBUG_WEAK)
                                fprintf (stderr, "forwarded from 0x%08x to 0x%08x\n",
                                                (uint)w->object,
                                                (uint)*(pointer*)w->object);
                        w->object = *(pointer*)w->object;
                } else {
                        if (DEBUG_WEAK)
                                fprintf (stderr, "cleared\n");
                        *(GC_getHeaderp((pointer)w)) = WEAK_GONE_HEADER;
                        w->object = (pointer)BOGUS_POINTER;
                }
        }
        s->weaks = NULL;
}

static void swapSemis (GC_state s) {
        struct GC_heap h;

        h = s->heap2;
        s->heap2 = s->heap;
        s->heap = h;
        setCardMapForMutator (s);
}

static inline bool detailedGCTime (GC_state s) {
        return s->summary;
}

static void cheneyCopy (GC_state s) {
        struct rusage ru_start;
        pointer toStart;

        assert (s->heap2.size >= s->oldGenSize);
        if (detailedGCTime (s))
                startTiming (&ru_start);
        s->numCopyingGCs++;
        s->toSpace = s->heap2.start;
        s->toLimit = s->heap2.start + s->heap2.size;
        if (DEBUG or s->messages) {
                fprintf (stderr, "Major copying GC.\n");
                fprintf (stderr, "fromSpace = 0x%08x of size %s\n", 
                                (uint) s->heap.start,
                                uintToCommaString (s->heap.size));
                fprintf (stderr, "toSpace = 0x%08x of size %s\n",
                                (uint) s->heap2.start,
                                uintToCommaString (s->heap2.size));
        }
        assert (s->heap2.start != (void*)NULL);
        /* The next assert ensures there is enough space for the copy to succeed.
         * It does not assert (s->heap2.size >= s->heap.size) because that
         * is too strong.
         */
        assert (s->heap2.size >= s->oldGenSize);
        toStart = alignFrontier (s, s->heap2.start);
        s->back = toStart;
        foreachGlobal (s, forward);
        foreachPointerInRange (s, toStart, &s->back, TRUE, forward);
        updateWeaks (s);
        s->oldGenSize = s->back - s->heap2.start;
        s->bytesCopied += s->oldGenSize;
        if (DEBUG)
                fprintf (stderr, "%s bytes live.\n", 
                                uintToCommaString (s->oldGenSize));
        swapSemis (s);
        clearCrossMap (s);
        s->lastMajor = GC_COPYING;
        if (detailedGCTime (s))
                stopTiming (&ru_start, &s->ru_gcCopy);          
        if (DEBUG or s->messages)
                fprintf (stderr, "Major copying GC done.\n");
}
