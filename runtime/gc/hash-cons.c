/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                       Object hash consing                        */
/* ---------------------------------------------------------------- */

/* Hashing based on Introduction to Algorithms by Cormen, Leiserson, and Rivest.
 * Section numbers in parens.
 * k is key to be hashed.
 * table is of size 2^p  (it must be a power of two)
 * Open addressing (12.4), meaning that we stick the entries directly in the 
 *   table and probe until we find what we want.
 * Multiplication method (12.3.2), meaning that we compute the hash by 
 *   multiplying by a magic number, chosen by Knuth, and take the high-order p
 *   bits of the low order 32 bits.
 * Double hashing (12.4), meaning that we use two hash functions, the first to
 *   decide where to start looking and a second to decide at what offset to
 *   probe.  The second hash must be relatively prime to the table size, which
 *   we ensure by making it odd and keeping the table size as a power of 2.
 */

static GC_ObjectHashTable newTable (GC_state s) {
        int i;
        uint maxElementsSize;
        pointer regionStart;
        pointer regionEnd;
        GC_ObjectHashTable t;

        NEW (GC_ObjectHashTable, t);
        // Try to use space in the heap for the elements.
        if (not (heapIsInit (&s->heap2))) {
                if (DEBUG_SHARE)
                        fprintf (stderr, "using heap2\n");
                // We have all of heap2 available.  Use it.
                regionStart = s->heap2.start;
                regionEnd = s->heap2.start + s->heap2.size;
        } else if (s->amInGC or not s->canMinor) {
                if (DEBUG_SHARE)
                        fprintf (stderr, "using end of heap\n");
                regionStart = s->frontier;
                regionEnd = s->limitPlusSlop;
        } else {
                if (DEBUG_SHARE)
                        fprintf (stderr, "using minor space\n");
                // Use the space available for a minor GC.
                assert (s->canMinor);
                regionStart = s->heap.start + s->oldGenSize;
                regionEnd = s->nursery;
        }
        maxElementsSize = (regionEnd - regionStart) / sizeof (*(t->elements));
        if (DEBUG_SHARE)
                fprintf (stderr, "maxElementsSize = %u\n", maxElementsSize);
        t->elementsSize = 64;  // some small power of two
        t->log2ElementsSize = 6;  // and its log base 2
        if (maxElementsSize < t->elementsSize) {
                if (DEBUG_SHARE)
                        fprintf (stderr, "too small -- using malloc\n");
                t->elementsIsInHeap = FALSE;
                ARRAY (struct GC_ObjectHashElement *, t->elements, t->elementsSize);
        } else {
                t->elementsIsInHeap = TRUE;
                t->elements = (struct GC_ObjectHashElement*)regionStart;
                // Find the largest power of two that fits.
                for (; t->elementsSize <= maxElementsSize; 
                        t->elementsSize <<= 1, t->log2ElementsSize++)
                        ; // nothing
                t->elementsSize >>= 1;
                t->log2ElementsSize--;
                assert (t->elementsSize <= maxElementsSize);
                for (i = 0; i < t->elementsSize; ++i)
                        t->elements[i].object = NULL;
        }
        t->numElements = 0;
        t->mayInsert = TRUE;
        if (DEBUG_SHARE) {
                fprintf (stderr, "elementsIsInHeap = %s\n", 
                                boolToString (t->elementsIsInHeap));
                fprintf (stderr, "elementsSize = %u\n", t->elementsSize);
                fprintf (stderr, "0x%08x = newTable ()\n", (uint)t);
        }
        return t;
}

static void destroyTable (GC_ObjectHashTable t) {
        unless (t->elementsIsInHeap)
                free (t->elements);
        free (t);
}

static inline Pointer tableInsert 
        (GC_state s, GC_ObjectHashTable t, W32 hash, Pointer object, 
                Bool mightBeThere, Header header, W32 tag, Pointer max) {
        GC_ObjectHashElement e;
        Header header2;
        static Bool init = FALSE;
        static int maxNumProbes = 0;
        static W64 mult; // magic multiplier for hashing
        int numProbes;
        W32 probe;
        word *p;
        word *p2;
        W32 slot; // slot in hash table we are considering

        if (DEBUG_SHARE)
                fprintf (stderr, "tableInsert (%u, 0x%08x, %s, 0x%08x, 0x%08x)\n",
                                (uint)hash, (uint)object, 
                                boolToString (mightBeThere),
                                (uint)header, (uint)max);
        if (! init) {
                init = TRUE;
                mult = floor (((sqrt (5.0) - 1.0) / 2.0)
                                * (double)0x100000000llu);
        }
        slot = (W32)(mult * (W64)hash) >> (32 - t->log2ElementsSize);
        probe = (1 == slot % 2) ? slot : slot - 1;
        if (DEBUG_SHARE)
                fprintf (stderr, "probe = 0x%08x\n", (uint)probe);
        assert (1 == probe % 2);
        numProbes = 0;
look:
        if (DEBUG_SHARE)
                fprintf (stderr, "slot = 0x%08x\n", (uint)slot);
        assert (0 <= slot and slot < t->elementsSize);
        numProbes++;
        e = &t->elements[slot];
        if (NULL == e->object) {
                /* It's not in the table.  Add it. */
                unless (t->mayInsert) {
                        if (DEBUG_SHARE)
                                fprintf (stderr, "not inserting\n");
                        return object;
                }
                e->hash = hash;
                e->object = object;
                t->numElements++;
                if (numProbes > maxNumProbes) {
                        maxNumProbes = numProbes;
                        if (DEBUG_SHARE)
                                fprintf (stderr, "numProbes = %d\n", numProbes);
                }
                return object;
        }
        unless (hash == e->hash) {
lookNext:
                slot = (slot + probe) % t->elementsSize;
                goto look;
        }
        unless (mightBeThere)
                goto lookNext;
        if (DEBUG_SHARE)
                fprintf (stderr, "comparing 0x%08x to 0x%08x\n",
                                (uint)object, (uint)e->object);
        /* Compare object to e->object. */
        unless (object == e->object) {
                header2 = GC_getHeader (e->object);
                unless (header == header2)
                        goto lookNext;
                for (p = (word*)object, p2 = (word*)e->object; 
                                p < (word*)max; 
                                ++p, ++p2)
                        unless (*p == *p2)
                                goto lookNext;
                if (ARRAY_TAG == tag
                        and (GC_arrayNumElements (object)
                                != GC_arrayNumElements (e->object)))
                        goto lookNext;
        }
        /* object is equal to e->object. */
        return e->object;
}

static void maybeGrowTable (GC_state s, GC_ObjectHashTable t) { 
        int i;
        GC_ObjectHashElement oldElement;
        struct GC_ObjectHashElement *oldElements;
        uint oldSize;
        uint newSize;

        if (not t->mayInsert or t->numElements * 2 <= t->elementsSize)
                return;
        oldElements = t->elements;
        oldSize = t->elementsSize;
        newSize = oldSize * 2;
        if (DEBUG_SHARE)
                fprintf (stderr, "trying to grow table to size %d\n", newSize);
        // Try to alocate the new table.
        ARRAY_UNSAFE (struct GC_ObjectHashElement *, t->elements, newSize);
        if (NULL == t->elements) {
                t->mayInsert = FALSE;
                t->elements = oldElements;
                if (DEBUG_SHARE)
                        fprintf (stderr, "unable to grow table\n");
                return;
        }
        t->elementsSize = newSize;
        t->log2ElementsSize++;
        for (i = 0; i < oldSize; ++i) {
                oldElement = &oldElements[i];
                unless (NULL == oldElement->object)
                        tableInsert (s, t, oldElement->hash, oldElement->object,
                                        FALSE, 0, 0, 0);
        }
        if (t->elementsIsInHeap)
                t->elementsIsInHeap = FALSE;
        else
                free (oldElements);
        if (DEBUG_SHARE)
                fprintf (stderr, "done growing table\n");
}

static Pointer hashCons (GC_state s, Pointer object, Bool countBytesHashConsed) {
        Bool hasIdentity;
        Word32 hash;
        Header header;
        pointer max;
        uint numNonPointers;
        uint numPointers;
        word *p;
        Pointer res;
        GC_ObjectHashTable t;
        uint tag;

        if (DEBUG_SHARE)
                fprintf (stderr, "hashCons (0x%08x)\n", (uint)object);
        t = s->objectHashTable;
        header = GC_getHeader (object);
        SPLIT_HEADER ();
        if (hasIdentity) {
                /* Don't hash cons. */
                res = object;
                goto done;
        }
        assert (ARRAY_TAG == tag or NORMAL_TAG == tag);
        max = object
                + (ARRAY_TAG == tag
                        ? arrayNumBytes (s, object,
                                                numPointers, numNonPointers)
                        : toBytes (numPointers + numNonPointers));
        // Compute the hash.
        hash = header;
        for (p = (word*)object; p < (word*)max; ++p)
                hash = hash * 31 + *p;
        /* Insert into table. */
        res = tableInsert (s, t, hash, object, TRUE, header, tag, (Pointer)max);
        maybeGrowTable (s, t);
        if (countBytesHashConsed and res != object) {
                uint amount;

                amount = max - object;
                if (ARRAY_TAG == tag)
                        amount += GC_ARRAY_HEADER_SIZE;
                else
                        amount += GC_NORMAL_HEADER_SIZE;
                s->bytesHashConsed += amount;
        }
done:
        if (DEBUG_SHARE)
                fprintf (stderr, "0x%08x = hashCons (0x%08x)\n", 
                                (uint)res, (uint)object);
        return res;
}

static inline void maybeSharePointer (GC_state s,
                                        Pointer *pp, 
                                        Bool shouldHashCons) {
        unless (shouldHashCons)
                return;
        if (DEBUG_SHARE)
                fprintf (stderr, "maybeSharePointer  pp = 0x%08x  *pp = 0x%08x\n",
                                (uint)pp, (uint)*pp);
        *pp = hashCons (s, *pp, FALSE); 
}

static void bytesHashConsedMessage (GC_state s, ullong total) {
        fprintf (stderr, "%s bytes hash consed (%.1f%%).\n",
                ullongToCommaString (s->bytesHashConsed),
                100.0 * ((double)s->bytesHashConsed / (double)total));
}

void GC_share (GC_state s, Pointer object) {
        W32 total;

        if (DEBUG_SHARE)
                fprintf (stderr, "GC_share 0x%08x\n", (uint)object);
        if (DEBUG_SHARE or s->messages)
                s->bytesHashConsed = 0;
        // Don't hash cons during the first round of marking.
        total = mark (s, object, MARK_MODE, FALSE);
        s->objectHashTable = newTable (s);
        // Hash cons during the second round of marking.
        mark (s, object, UNMARK_MODE, TRUE);
        destroyTable (s->objectHashTable);
        if (DEBUG_SHARE or s->messages)
                bytesHashConsedMessage (s, total);
}
