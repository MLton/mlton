/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void loadWorldFromFD (GC_state s, FILE *f);
static void loadWorldFromFileName (GC_state s, const char *fileName);
static int saveWorldToFD (GC_state s, FILE *f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

/* 0 = success, 1 = failure (a bool) */
uint32_t GC_saveWorld (GC_state s, NullString8_t fileName);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
