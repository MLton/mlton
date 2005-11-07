/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

uint32_t GC_getArrayLength (pointer a);

void GC_handler (GC_state s, int signum);

void GC_pack (GC_state s);
void GC_unpack (GC_state s);

void GC_share (GC_state s, pointer object);

size_t GC_size (GC_state s, pointer root);

void GC_startHandler (GC_state s);
void GC_finishHandler (GC_state s);

void GC_switchToThread (GC_state s, GC_thread t, size_t ensureBytesFree);


GC_profileData GC_getProfileCurrent (GC_state s);
void GC_setProfileCurrent (GC_state s, GC_profileData p);

void GC_profileFree (GC_state s, GC_profileData p);
GC_profileData GC_profileNew (GC_state s);
void GC_profileWrite (GC_state s, GC_profileData p, int fd);

void GC_profileDone (GC_state s);

void GC_done (GC_state s);

