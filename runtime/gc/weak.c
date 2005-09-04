/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


bool GC_weakCanGet (pointer p) {
        Bool res;

        res = WEAK_GONE_HEADER != GC_getHeader (p);
        if (DEBUG_WEAK)
                fprintf (stderr, "%s = GC_weakCanGet (0x%08x)\n",
                                boolToString (res), (uint)p);
        return res;
}

Pointer GC_weakGet (Pointer p) {
        pointer res;

        res = ((GC_weak)p)->object;
        if (DEBUG_WEAK)
                fprintf (stderr, "0x%08x = GC_weakGet (0x%08x)\n",
                                (uint)res, (uint)p);
        return res;
}

Pointer GC_weakNew (GC_state s, Word32 header, Pointer p) {
        pointer res;

        res = object (s, header, GC_NORMAL_HEADER_SIZE + 3 * WORD_SIZE, 
                        FALSE, FALSE);
        ((GC_weak)res)->object = p;
        if (DEBUG_WEAK)
                fprintf (stderr, "0x%08x = GC_weakNew (0x%08x, 0x%08x)\n",
                                (uint)res, (uint)header, (uint)p);
        return res;
}
