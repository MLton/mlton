/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_COMMON_H_
#define _C_COMMON_H_

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef DEBUG_CCODEGEN
#define DEBUG_CCODEGEN FALSE
#endif

#include "export.h"

typedef uintptr_t ChunkFn_t (CPointer, CPointer, CPointer, uintptr_t);
typedef ChunkFn_t *ChunkFnPtr_t;

#endif /* #ifndef _C_COMMON_H_ */
