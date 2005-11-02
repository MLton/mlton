/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void updateWeaksForCheneyCopy (GC_state s);
void swapHeapsForCheneyCopy (GC_state s);
void majorCheneyCopyGC (GC_state s);
void minorCheneyCopyGC (GC_state s);
