/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void loadWorldFromFILE (GC_state s, FILE *f);
static void loadWorldFromFileName (GC_state s, const char *fileName);
static int saveWorldToFILE (GC_state s, FILE *f);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_saveWorld (GC_state s, NullString8_t fileName);
/* TRUE = success, FALSE = failure */
C_Errno_t(Bool_t) GC_getSaveWorldStatus (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
