/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * Weak objects have the following layout:
 * 
 * header word32 ::
 * padding ::
 * link native-pointer ::
 * object-pointer
 *
 * The object type indexed by the header determines whether the weak
 * is valid or not.  If the type has numPointers == 1, then the weak
 * pointer is valid.  Otherwise, the type has numPointers == 0 and the
 * weak pointer is not valid.
 *
 * There may be zero or more bytes of padding for alignment purposes.
 *
 * The link native-pointer is used to chain the live weaks together
 * during a copying gc and is otherwise unused.
 *
 * The final component is the weak object-pointer.
 */ 
typedef struct GC_weak {
  struct GC_weak *link;
  objptr objptr;
} *GC_weak;

size_t sizeofWeak (GC_state s);
size_t offsetofWeak (GC_state s);
uint32_t GC_weakCanGet (GC_state s, pointer p);
pointer GC_weakGet (GC_state s, pointer p);
pointer GC_weakNew (GC_state s, GC_header header, pointer p);

