/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void loadWorldFromFD (GC_state s, int fd);
void loadWorldFromFileName (GC_state s, char *fileName);
void saveWorldToFD (GC_state s, int fd);
void GC_saveWorldToFD (GC_state s, int fd);

