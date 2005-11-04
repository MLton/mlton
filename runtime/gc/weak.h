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
 * header word ::
 * unused word ::
 * link word ::
 * heap-pointer
 *
 * The object type indexed by the header determines whether the weak
 * is valid or not.  If the type has numPointers == 1, then the weak
 * pointer is valid.  Otherwise, the type has numPointers == 0 and the
 * weak pointer is not valid.
 *
 * The first word is unused; present for alignment purposes
 *
 * The second word is used to chain the live weaks together during a copying gc
 * and is otherwise unused.
 *
 * The third word is the weak pointer.
 */ 
typedef struct GC_weak {
  uint32_t unused;
  struct GC_weak *link;
  objptr objptr;
} *GC_weak;

size_t sizeofWeak (GC_state s);
uint32_t GC_weakCanGet (GC_state s, pointer p);
pointer GC_weakGet (GC_state s, pointer p);
pointer GC_weakNew (GC_state s, GC_header header, pointer p);

