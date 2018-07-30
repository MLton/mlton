/* Copyright (C) 2018 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * Allocates a sequence of objptrs with the given header and length in the heap,
 * sets the frontier beyond it, and returns the beginning of the object
 * 
 * The bytes_noalign argument is aligned in the method.
 */
GC_objptr_sequence allocate_objptr_seq (GC_state s,
                                        GC_header h,
                                        GC_sequenceLength argct) {
  // compute the size and align it
  size_t bytes = align(argct * sizeof(objptr) + GC_SEQUENCE_METADATA_SIZE, s->alignment);

  GC_objptr_sequence seq = (GC_objptr_sequence)s->frontier;  // create object
  
  // set sequence parameters
  seq->counter = (GC_sequenceCounter)0;
  seq->length  = argct;
  seq->header  = h;

  // set the frontier and return the new object (a pointer into the heap)
  setFrontier (s, (pointer)&seq->objs[(int)argct], bytes);
  return seq;
}
