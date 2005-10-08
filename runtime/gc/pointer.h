/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef unsigned char* pointer;
#define POINTER_SIZE sizeof(pointer)
#define FMTPTR "0x%016"PRIxPTR
#define BOGUS_POINTER (pointer)0x1

#define WORD_SIZE POINTER_SIZE
