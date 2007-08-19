/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

// typedef void* pointer;
// typedef unsigned char* pointer;
typedef unsigned char pointerAux __attribute__ ((aligned (4), may_alias));
typedef pointerAux* pointer;

#define POINTER_SIZE sizeof(pointer)
#if defined(__WORDSIZE)
#if __WORDSIZE == 32
#define FMTPTR "0x%08"PRIxPTR
#elif __WORDSIZE == 64
#define FMTPTR "0x%016"PRIxPTR
#else
#error __WORDSIZE unknown
#endif
#elif defined(__LP64__)
#define FMTPTR "0x%016"PRIxPTR
#else
#define FMTPTR "0x%08"PRIxPTR
#endif

typedef const unsigned char* code_pointer;
