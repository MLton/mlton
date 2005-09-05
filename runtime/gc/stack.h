/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * Stack objects have the following layout:
 * 
 * header word ::
 * markTop pointer ::
 * markIndex word ::
 * reserved word ::
 * used word ::
 * ... reserved bytes ...
 *
 * The markTop pointer and markIndex word are used by mark compact GC.
 * The reserved word gives the number of bytes for the stack (before
 * the next ML object).  The used word gives the number of bytes
 * currently used by the stack.  The sequence of reserved bytes
 * correspond to ML stack frames, which will be discussed in more
 * detail in "frame.h".
*/
typedef struct GC_stack {       
  /* markTop and markIndex are only used during marking.  They record
   * the current pointer in the stack that is being followed.  markTop
   * points to the top of the stack frame containing the pointer and
   * markIndex is the index in that frames frameOffsets of the pointer
   * slot.  So, when the GC pointer reversal gets back to the stack,
   * it can continue with the next pointer (either in the current
   * frame or the next frame).
   */
  pointer markTop;
  uint32_t markIndex;
  /* reserved is the number of bytes reserved for stack, 
   * i.e. its maximum size.
   */
  uint32_t reserved;
  /* used is the number of bytes used by the stack.  
   * Stacks with used == reserved are continuations.
   */
  uint32_t used;      
  /* The next address is the bottom of the stack, and the following
   * reserved bytes hold space for the stack.
   */
} *GC_stack;
