/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_pack (GC_state s);
void GC_unpack (GC_state s);

void GC_share (GC_state s, pointer object);

size_t GC_size (GC_state s, pointer root);


int GC_init (GC_state s, int argc, char **argv);
void GC_done (GC_state s);
