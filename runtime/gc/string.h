/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Layout of strings.  
 * Note, the value passed around is a pointer to the chars member.
 */
typedef struct GC_string8 {
  GC_arrayCounter counter;
  GC_arrayLength length;
  GC_header header;
  union {
    char c[1];
    pointerAux p;
  } chars;
} *GC_string8;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define GC_STRING8_HEADER GC_WORD8_VECTOR_HEADER

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
