/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef struct GC_intInf {
  GC_arrayCounter counter;
  GC_arrayLength length;
  GC_header header;
  uint32_t isneg;
  uint32_t *limbs;
} *GC_intInf;
