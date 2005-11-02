/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                       Depth-first Marking                        */
/* ---------------------------------------------------------------- */

bool isMarked (pointer p) {
  return MARK_MASK & getHeader (p);
}

bool isMarkedMode (GC_markMode m, pointer p) {
  switch (m) {
  case MARK_MODE:
    return isMarked (p);
  case UNMARK_MODE:
    return not isMarked (p);
  default:
    die ("bad mark mode %u", m);
  }
}

pointer arrayIndexAtPointer (GC_state s, pointer a,
                             GC_arrayCounter arrayIndex,
                             uint32_t pointerIndex) {
  GC_header header;
  uint16_t numNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  
  header = getHeader (a);
  splitHeader(s, header, &tag, NULL, &numNonObjptrs, &numObjptrs);
  assert (tag == ARRAY_TAG);

  size_t nonObjptrBytesPerElement =
    sizeofNumNonObjptrs (ARRAY_TAG, numNonObjptrs);
  size_t bytesPerElement =
    nonObjptrBytesPerElement
    + (numObjptrs * OBJPTR_SIZE);

  return a
    + arrayIndex * bytesPerElement
    + nonObjptrBytesPerElement
    + pointerIndex * OBJPTR_SIZE;
}

/* dfsMark (s, r, m, shc) 
 *
 * Sets all the mark bits in the object graph pointed to by r. 
 *
 * If m is MARK_MODE, it sets the bits to 1.
 * If m is UNMARK_MODE, it sets the bits to 0.
 *
 * If shc, it hash-conses the objects marked.
 *
 * It returns the total size in bytes of the objects marked.
 */
size_t dfsMark (GC_state s, pointer root,
                GC_markMode mode, bool shouldHashCons) {
  GC_header mark; /* Used to set or clear the mark bit. */
  size_t size; /* Total number of bytes marked. */
  pointer cur; /* The current object being marked. */
  pointer prev; /* The previous object on the mark stack. */
  pointer next; /* The next object to mark. */
  pointer todo; /* A pointer to the pointer in cur to next. */
  GC_header header;
  GC_header* headerp;
  uint16_t numNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  uint32_t index; /* The i'th pointer in the object (element) being marked. */
  GC_header nextHeader;
  GC_header* nextHeaderp;
  GC_arrayCounter arrayIndex;
  pointer top; /* The top of the next stack frame to mark. */
  GC_returnAddress returnAddress; 
  GC_frameLayout frameLayout;
  GC_frameOffsets frameOffsets;

  if (isMarkedMode (mode, root))
    /* Object has already been marked. */
    return 0;
  mark = (MARK_MODE == mode) ? MARK_MASK : 0;
  size = 0;
  cur = root;
  prev = NULL;
  headerp = getHeaderp (cur);
  header = *headerp;
  goto mark;      
markNext:
  /* cur is the object that was being marked.
   * prev is the mark stack.
   * next is the unmarked object to be marked.
   * nextHeaderp points to the header of next.
   * nextHeader is the header of next.
   * todo is a pointer to the pointer inside cur that points to next.
   */
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, 
             "markNext"
             "  cur = "FMTPTR"  next = "FMTPTR
             "  prev = "FMTPTR"  todo = "FMTPTR"\n",
             (uintptr_t)cur, (uintptr_t)next, 
             (uintptr_t)prev, (uintptr_t)todo);
  assert (not isMarkedMode (mode, next));
  assert (nextHeaderp == getHeaderp (next));
  assert (nextHeader == getHeader (next));
  // assert (*(pointer*) todo == next);
  assert (fetchObjptrToPointer (todo, s->heap.start) == next);
  headerp = nextHeaderp;
  header = nextHeader;
  // *(pointer*)todo = prev;
  storeObjptrFromPointer (todo, prev, s->heap.start);
  prev = cur;
  cur = next;
mark:
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "mark  cur = "FMTPTR"  prev = "FMTPTR"  mode = %s\n",
             (uintptr_t)cur, (uintptr_t)prev,
             (mode == MARK_MODE) ? "mark" : "unmark");
  /* cur is the object to mark. 
   * prev is the mark stack.
   * headerp points to the header of cur.
   * header is the header of cur.
   */
  assert (not isMarkedMode (mode, cur));
  assert (header == getHeader (cur));
  assert (headerp == getHeaderp (cur));
  header ^= MARK_MASK;
  /* Store the mark.  In the case of an object that contains a pointer to
   * itself, it is essential that we store the marked header before marking
   * the internal pointers (markInNormal below).  If we didn't, then we
   * would see the object as unmarked and traverse it again.
   */
  *headerp = header;
  splitHeader (s, header, &tag, NULL, &numNonObjptrs, &numObjptrs);
  if (NORMAL_TAG == tag) {
    size += 
      GC_NORMAL_HEADER_SIZE 
      + sizeofNumNonObjptrs (tag, numNonObjptrs) 
      + (numObjptrs * OBJPTR_SIZE);
    if (0 == numObjptrs) {
      /* There is nothing to mark. */
normalDone:
      if (shouldHashCons)
        cur = hashConsPointer (s, cur, TRUE);
      goto ret;
    }
    todo = cur + sizeofNumNonObjptrs (NORMAL_TAG, numNonObjptrs);
    index = 0;
markInNormal:
    if (DEBUG_MARK_COMPACT)
      fprintf (stderr, "markInNormal  index = %"PRIu32"\n", index);
    assert (index < numObjptrs);
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (todo, s->heap.start);
    if (not isPointer (next)) {
markNextInNormal:
      assert (index < numObjptrs);
      index++;
      if (index == numObjptrs) {
        /* Done.  Clear out the counters and return. */
        *headerp = header & ~COUNTER_MASK;
        goto normalDone;
      }
      todo += OBJPTR_SIZE;
      goto markInNormal;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      if (shouldHashCons)
        shareObjptr (s, (objptr*)todo);
      goto markNextInNormal;
    }
    *headerp = (header & ~COUNTER_MASK) | (index << COUNTER_SHIFT);
    goto markNext;
  } else if (WEAK_TAG == tag) {
    /* Store the marked header and don't follow any pointers. */
    goto ret;
  } else if (ARRAY_TAG == tag) {
    /* When marking arrays:
     *   arrayIndex is the index of the element to mark.
     *   cur is the pointer to the array.
     *   index is the index of the pointer within the element
     *     (i.e. the i'th pointer is at index i).
     *   todo is the start of the element.
     */
    size += 
      GC_ARRAY_HEADER_SIZE
      + sizeofArrayNoHeader (s, getArrayLength (cur), numNonObjptrs, numObjptrs);
    if (0 == numObjptrs or 0 == getArrayLength (cur)) {
      /* There is nothing to mark. */
arrayDone:
      if (shouldHashCons)
        cur = hashConsPointer (s, cur, TRUE);
      goto ret;
    }
    /* Begin marking first element. */
    arrayIndex = 0;
    todo = cur;
markArrayElt:
    assert (arrayIndex < getArrayLength (cur));
    index = 0;
    /* Skip to the first pointer. */
    todo += sizeofNumNonObjptrs (ARRAY_TAG, numNonObjptrs);
markInArray:
    if (DEBUG_MARK_COMPACT)
      fprintf (stderr, "markInArray arrayIndex = %"PRIu32" index = %"PRIu32"\n",
               arrayIndex, index);
    assert (arrayIndex < getArrayLength (cur));
    assert (index < numObjptrs);
    assert (todo == arrayIndexAtPointer (s, cur, arrayIndex, index));
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (todo, s->heap.start);
    if (not (isPointer(next))) {
markNextInArray:
      assert (arrayIndex < getArrayLength (cur));
      assert (index < numObjptrs);
      assert (todo == arrayIndexAtPointer (s, cur, arrayIndex, index));
      todo += OBJPTR_SIZE;
      index++;
      if (index < numObjptrs)
        goto markInArray;
      arrayIndex++;
      if (arrayIndex < getArrayLength (cur))
        goto markArrayElt;
      /* Done.  Clear out the counters and return. */
      *getArrayCounterp (cur) = 0;
      *headerp = header & ~COUNTER_MASK;
      goto arrayDone;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      if (shouldHashCons)
        shareObjptr (s, (objptr*)todo);
      goto markNextInArray;
    }
    /* Recur and mark next. */
    *getArrayCounterp (cur) = arrayIndex;
    *headerp = (header & ~COUNTER_MASK) | (index << COUNTER_SHIFT);
    goto markNext;
  } else {
    assert (STACK_TAG == tag);
    size += 
      GC_STACK_HEADER_SIZE
      + sizeof (struct GC_stack) + ((GC_stack)cur)->reserved;
    top = getStackTop (s, (GC_stack)cur);
    assert (((GC_stack)cur)->used <= ((GC_stack)cur)->reserved);
markInStack:
    /* Invariant: top points just past the return address of the frame
     * to be marked.
     */
    assert (getStackBottom (s, (GC_stack)cur) <= top);
    if (DEBUG_MARK_COMPACT)
      fprintf (stderr, "markInStack  top = %zu\n",
               (size_t)(top - getStackBottom (s, (GC_stack)cur)));
    if (top == getStackBottom (s, (GC_stack)(cur)))
      goto ret;
    index = 0;
    returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
    frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
    frameOffsets = frameLayout->offsets;
    ((GC_stack)cur)->markTop = top;
markInFrame:
    if (index == frameOffsets [0]) {
      top -= frameLayout->size;
      goto markInStack;
    }
    todo = top - frameLayout->size + frameOffsets [index + 1];
    // next = *(pointer*)todo;
    next = fetchObjptrToPointer (todo, s->heap.start);
    if (DEBUG_MARK_COMPACT)
      fprintf (stderr,
               "    offset %u  todo "FMTPTR"  next = "FMTPTR"\n",
               frameOffsets [index + 1],
               (uintptr_t)todo, (uintptr_t)next);
    if (not isPointer (next)) {
      index++;
      goto markInFrame;
    }
    nextHeaderp = getHeaderp (next);
    nextHeader = *nextHeaderp;
    if (mark == (nextHeader & MARK_MASK)) {
      index++;
      if (shouldHashCons)
        shareObjptr (s, (objptr*)todo);
      goto markInFrame;
    }
    ((GC_stack)cur)->markIndex = index;
    goto markNext;
  }
  assert (FALSE);
ret:
  /* Done marking cur, continue with prev.
   * Need to set the pointer in the prev object that pointed to cur 
   * to point back to prev, and restore prev.
   */
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "return  cur = "FMTPTR"  prev = "FMTPTR"\n",
             (uintptr_t)cur, (uintptr_t)prev);
  assert (isMarkedMode (mode, cur));
  if (NULL == prev)
    return size;
  next = cur;
  cur = prev;
  headerp = getHeaderp (cur);
  header = *headerp;
  splitHeader (s, header, &tag, NULL, &numNonObjptrs, &numObjptrs);
  /* It's impossible to get a WEAK_TAG here, since we would never
   * follow the weak object pointer.
   */
  assert (WEAK_TAG != tag);
  if (NORMAL_TAG == tag) {
    todo = cur + sizeofNumNonObjptrs (tag, numNonObjptrs);
    index = (header & COUNTER_MASK) >> COUNTER_SHIFT;
    todo += index * OBJPTR_SIZE;
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (todo, s->heap.start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap.start);
    goto markNextInNormal;
  } else if (ARRAY_TAG == tag) {
    arrayIndex = getArrayCounter (cur);
    todo = cur + arrayIndex * (sizeofNumNonObjptrs (ARRAY_TAG, numNonObjptrs)
                               + (numObjptrs * OBJPTR_SIZE));
    index = (header & COUNTER_MASK) >> COUNTER_SHIFT;
    todo += sizeofNumNonObjptrs (ARRAY_TAG, numNonObjptrs) + index * OBJPTR_SIZE;
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (todo, s->heap.start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap.start);
    goto markNextInArray;
  } else {
    assert (STACK_TAG == tag);
    index = ((GC_stack)cur)->markIndex;
    top = ((GC_stack)cur)->markTop;
    /* Invariant: top points just past a "return address". */
    returnAddress = *(GC_returnAddress*) (top - GC_RETURNADDRESS_SIZE);
    frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
    frameOffsets = frameLayout->offsets;
    todo = top - frameLayout->size + frameOffsets [index + 1];
    // prev = *(pointer*)todo;
    prev = fetchObjptrToPointer (todo, s->heap.start);
    // *(pointer*)todo = next;
    storeObjptrFromPointer (todo, next, s->heap.start);
    index++;
    goto markInFrame;
  }
  assert (FALSE);
}

void dfsMarkTrue (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  dfsMark (s, p, MARK_MODE, TRUE);
}

void dfsMarkFalse (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  dfsMark (s, p, MARK_MODE, FALSE);
}

void dfsUnmark (GC_state s, objptr *opp) {
  pointer p;

  p = objptrToPointer (*opp, s->heap.start);
  dfsMark (s, p, UNMARK_MODE, FALSE);
}
