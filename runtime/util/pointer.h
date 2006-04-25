/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef unsigned char* pointer;
// typedef void* pointer;
#define POINTER_SIZE sizeof(pointer)
#define FMTPTR "0x%016"PRIxPTR
