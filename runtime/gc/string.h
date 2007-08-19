/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Layout of strings.  
 * Note, the value passed around is a pointer to the obj member.
 */
typedef struct GC_string8 {
  GC_arrayCounter counter;
  GC_arrayLength length;
  GC_header header;
  union {
    struct {
      char chars[1];
    } body;
    pointerAux _p; /* alignment */
  } obj;
} __attribute__ ((packed)) *GC_string8;

COMPILE_TIME_ASSERT(GC_string8__obj_packed,
                    offsetof(struct GC_string8, obj) == 
                    sizeof(GC_arrayCounter) 
                    + sizeof(GC_arrayLength) 
                    + sizeof(GC_header));
COMPILE_TIME_ASSERT(GC_string8__obj_body_chars_packed,
                    offsetof(struct GC_string8, obj.body.chars) == 
                    offsetof(struct GC_string8, obj));

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define GC_STRING8_HEADER GC_WORD8_VECTOR_HEADER

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
