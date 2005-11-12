/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* Layout of strings.  
 * Note, the value passed around is a pointer to the chars member.
 */
typedef struct GC_string {
  GC_arrayCounter counter;
  GC_arrayLength length;
  GC_header header;
  char chars[1];
} *GC_string;

#define GC_STRING_HEADER GC_WORD8_VECTOR_HEADER
