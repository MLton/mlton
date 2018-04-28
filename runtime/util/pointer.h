/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

// typedef void* pointer;
// typedef unsigned char* pointer;
typedef unsigned char pointerAux __attribute__ ((may_alias));
typedef pointerAux* pointer;

#if POINTER_BITS == 32
#define FMTPTR "0x%08"PRIxPTR
#elif POINTER_BITS == 64
#define FMTPTR "0x%016"PRIxPTR
#else
#error POINTER_BITS undefined
#endif

typedef const unsigned char* code_pointer;
