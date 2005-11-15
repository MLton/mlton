/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void loadWorldFromFD (GC_state s, int fd);
static void loadWorldFromFileName (GC_state s, char *fileName);
static void saveWorldToFD (GC_state s, int fd);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_saveWorld (GC_state s, int fd);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
