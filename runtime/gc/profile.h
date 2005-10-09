/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_profileAllocInc (GC_state s, size_t bytes);

void GC_profileDone (GC_state s);

void GC_profileEnter (GC_state s);

// void GC_profileFree (GC_state s, GC_profile p);

void GC_profileInc (GC_state s, size_t bytes);

void GC_profileLeave (GC_state s);

// GC_profile GC_profileNew (GC_state s);

// void GC_profileWrite (GC_state s, GC_profile p, int fd);
