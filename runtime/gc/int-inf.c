/* Copyright (C) 2012,2014 Matthew Fluet.
 * Copyright (C) 1999-2005, 2007-2008 Henry Cejtin, Matthew Fluet,
 *    Suresh Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * Test if a intInf is a fixnum.
 */
static inline bool isSmall (objptr arg) {
  return (arg & 1);
}

static inline bool areSmall (objptr arg1, objptr arg2) {
  return ((arg1 & arg2) & (objptr)1);
}

/*
 * Convert a bignum intInf to a bignum pointer.
 */
static inline GC_intInf toBignum (GC_state s, objptr arg) {
  GC_intInf bp;

  assert (not isSmall(arg));
  bp = (GC_intInf)(objptrToPointer(arg, s->heap.start)
                   - (offsetof(struct GC_intInf, obj)
                      + offsetof(struct GC_intInf_obj, isneg)));
  if (DEBUG_INT_INF)
    fprintf (stderr, "bp->header = "FMTHDR"\n", bp->header);
  assert (bp->header == GC_INTINF_HEADER);
  return bp;
}

/*
 * Given an intInf, a pointer to an __mpz_struct and space large
 * enough to contain LIMBS_PER_OBJPTR + 1 limbs, fill in the
 * __mpz_struct.
 */
void fillIntInfArg (GC_state s, objptr arg, __mpz_struct *res,
                    mp_limb_t space[LIMBS_PER_OBJPTR + 1]) {
  GC_intInf bp;

  if (DEBUG_INT_INF)
    fprintf (stderr, "fillIntInfArg ("FMTOBJPTR", "FMTPTR", "FMTPTR")\n",
             arg, (uintptr_t)res, (uintptr_t)space);
  if (isSmall(arg)) {
    res->_mp_alloc = LIMBS_PER_OBJPTR + 1;
    res->_mp_d = space;
    if (arg == (objptr)1) {
      res->_mp_size = 0;
    } else {
      const objptr highBitMask = (objptr)1 << (CHAR_BIT * OBJPTR_SIZE - 1);
      bool neg = (arg & highBitMask) != (objptr)0;
      if (neg) {
        arg = -((arg >> 1) | highBitMask);
      } else {
        arg = (arg >> 1);
      }
      int size;
      if (sizeof(objptr) <= sizeof(mp_limb_t)) {
        space[0] = (mp_limb_t)arg;
        size = 1;
      } else {
        size = 0;
        while (arg != 0) {
          space[size] = (mp_limb_t)arg;
          // The conditional below is to quell a gcc warning:
          //   right shift count >= width of type
          // When (sizeof(objptr) <= sizeof(mp_limb_t)),
          // this branch is unreachable,
          // so the shift doesn't matter.
          arg = arg >> (sizeof(objptr) <= sizeof(mp_limb_t) ?
                        0 : CHAR_BIT * sizeof(mp_limb_t));
          size++;
        }
      }
      if (neg)
        size = - size;
      res->_mp_size = size;
    }
  } else {
    bp = toBignum (s, arg);
    /* The _mp_alloc field is declared as int.  
     * No possibility of an overflowing assignment, as all *huge*
     * intInfs must have come from some previous GnuMP evaluation.
     */
    res->_mp_alloc = (int)(bp->length - 1);
    res->_mp_d = (mp_limb_t*)(bp->obj.limbs);
    res->_mp_size = bp->obj.isneg ? - res->_mp_alloc : res->_mp_alloc;
  }
  assert ((res->_mp_size == 0)
          or (res->_mp_d[(res->_mp_size < 0
                          ? - res->_mp_size
                          : res->_mp_size) - 1] != 0));
  if (DEBUG_INT_INF_DETAILED)
    fprintf (stderr, "arg --> %s\n",
             mpz_get_str (NULL, 10, res));
}

/*
 * Initialize an __mpz_struct to use the space provided by the heap.
 */
void initIntInfRes (GC_state s, __mpz_struct *res,
                    ARG_USED_FOR_ASSERT size_t bytes) {
  GC_intInf bp;
  size_t nlimbs;

  assert (bytes <= (size_t)(s->limitPlusSlop - s->frontier));
  bp = (GC_intInf)s->frontier;
  /* We have as much space for the limbs as there is to the end of the
   * heap.  Divide by (sizeof(mp_limb_t)) to get number of limbs.
   */
  nlimbs = ((size_t)(s->limitPlusSlop - (pointer)bp->obj.limbs)) / (sizeof(mp_limb_t));
  /* The _mp_alloc field is declared as int. 
   * Avoid an overflowing assignment, which could happen with huge
   * heaps.
   */
  res->_mp_alloc = (int)(min(nlimbs,(size_t)INT_MAX));
  res->_mp_d = (mp_limb_t*)(bp->obj.limbs);
  res->_mp_size = 0; /* is this necessary? */
}

/*
 * Mimics the result initialization for two simultaneous results, but returns the
 * allocated sequence that will house the results
 */
GC_objptr_sequence initIntInfRes_2 (GC_state s,
                                    __mpz_struct *lres, __mpz_struct *rres,
                                    ARG_USED_FOR_ASSERT size_t lbytes,
                                    ARG_USED_FOR_ASSERT size_t rbytes) {
  GC_objptr_sequence seq = allocate_objptr_seq (s, GC_INTINF_VECTOR_HEADER, 2);

  GC_intInf bp;
  size_t nl_limbs, nr_limbs;  // number of limbs that each result could need

  assert (lbytes + rbytes <= (size_t)(s->limitPlusSlop - s->frontier));
  // ensure that this object only looks at area after the left object
  bp = (GC_intInf)(s->frontier + lbytes);
  /* Amount of space for first result is amount required by the size, no more.
   * Will need additional guaranteed heap space for the second result.
   */
  nl_limbs = lbytes / sizeof(mp_limb_t);
  /* Amount of space for second result limbs is the amount of space
   * from the end of the first until the end of the heap, since we don't need
   * further checking for another result.
   */
  nr_limbs = ((size_t)(s->limitPlusSlop - (pointer)bp->obj.limbs)) / sizeof(mp_limb_t);
  /* The _mp_alloc field is declared as int.
   * Avoid overflowing assignments, which could happen with huge
   * heaps.
   */
  lres->_mp_alloc = (int)(min(nl_limbs,(size_t)INT_MAX));
  rres->_mp_alloc = (int)(min(nr_limbs,(size_t)INT_MAX));
  lres->_mp_d = (mp_limb_t*)((GC_intInf)s->frontier)->obj.limbs;
  rres->_mp_d = (mp_limb_t*)bp->obj.limbs;
  /* is this necessary? */
  lres->_mp_size = 0;
  rres->_mp_size = 0;

  return seq;
}

/*
 * Simple checks and modular functions for results used in the
 * one- and two- final result methods
 */
/*
 * Performs a simple assertion that an __mpz_struct is in the correct
 * format before proceeding with result processing
 */
static inline void __mpz_is_correct_fmt (__mpz_struct *res) {
  assert ((res->_mp_size == 0)
          or (res->_mp_d[(res->_mp_size < 0
                          ? - res->_mp_size
                          : res->_mp_size) - 1] != 0)); 
}

/*
 * Performs a simple conversion to a GC_intInf so that it is lined up
 * correctly with the result
 * 
 * Returns res->mp_size after adjusting for negatives
 */
static inline int __mpz_intInf_lineup (__mpz_struct *res, GC_intInf *ii) {
  *ii = (GC_intInf)((pointer)res->_mp_d
                    - (offsetof(struct GC_intInf, obj)
                        + offsetof(struct GC_intInf_obj, limbs)));
  assert (res->_mp_d == (mp_limb_t*)((*ii)->obj.limbs));

  // adjust the size if necessary
  int size = res->_mp_size;
  if (size < 0) {
    (*ii)->obj.isneg = TRUE;
    size = - size;
  } else
    (*ii)->obj.isneg = FALSE;

  return size;
}

/*
 * Prepares a GC_intInf for output and sets the frontier once we
 * know those things are necessary
 */ 
static inline objptr __mpz_form_final (GC_state s, GC_intInf bp, int size, size_t bytes) {
  setFrontier (s, (pointer)(&bp->obj.limbs[size]), bytes);
  bp->counter = (GC_sequenceCounter)0;
  bp->length = (GC_sequenceLength)(size + 1); // +1 for isneg field
  bp->header = GC_INTINF_HEADER;

  return pointerToObjptr ((pointer)&bp->obj, s->heap.start);
}

/*
 * Determines if the __mpz_struct value is small enough to fit in an
 * object pointer; if so, sets the value pointer to it and returns 1.
 * Otherwise, returns 0.
 * 
 * Takes in a GC_intInf that has been properly aligned to the value
 * of res via a call to __mpz_intInf_lineup.
 */
static inline int __mpz_is_small (GC_intInf bp,
                                  int size,
                                  objptr *value) {
  assert (size >= 0);
  
  if (size == 0) {
    *value = (objptr)1;
    return 1;  // value must be small
  }

  // value is nonzero, but still fits in an objptr
  if (size <= LIMBS_PER_OBJPTR) {
    if (sizeof(objptr) <= sizeof(mp_limb_t)) {
      objptr ans;
      mp_limb_t val = bp->obj.limbs[0];
      if (bp->obj.isneg) {
        // We only fit if val in [1, 2^(CHAR_BIT * OBJPTR_SIZE - 2)].
        ans = (objptr)(- val);
        val = val - 1;
      } else {
        // We only fit if val in [0, 2^(CHAR_BIT * OBJPTR_SIZE - 2) - 1].
        ans = (objptr)val;
      }
      // The conditional below is to quell a gcc warning:
      //   right shift count >= width of type
      // When (sizeof(objptr) > sizeof(mp_limb_t)),
      // this branch is unreachable,
      // so the shift doesn't matter.
      if (val < (mp_limb_t)1<<(sizeof(objptr) > sizeof(mp_limb_t) ?
                               0 : CHAR_BIT * OBJPTR_SIZE - 2)) {
        *value = ans<<1 | 1;
        return 1;
      }
    } else {
      objptr ans, val;
      val = (objptr)(bp->obj.limbs[0]);
      for (int i = 1; i < size; i++) {
        // The conditional below is to quell a gcc warning:
        //   left shift count >= width of type
        // When (sizeof(objptr) <= sizeof(mp_limb_t)),
        // this branch is unreachable,
        // so the shift doesn't matter.
        val = val << (sizeof(objptr) <= sizeof(mp_limb_t) ?
                      0 : CHAR_BIT * sizeof(mp_limb_t));
        val = val & (objptr)(bp->obj.limbs[i]);
      }
      if (bp->obj.isneg) {
        // We only fit if val in [1, 2^(CHAR_BIT * OBJPTR_SIZE - 2)].
        ans = - val;
        val = val - 1;
      } else {
        // We only fit if val in [0, 2^(CHAR_BIT * OBJPTR_SIZE - 2) - 1].
        ans = val;
      }
      if (val < (objptr)1<<(CHAR_BIT * OBJPTR_SIZE - 2)) {
        *value = ans<<1 | 1;
        return 1;
      }
    }
  }
  // the result is not small - the frontier will have to be set
  return 0;
}

/*
 * Given an __mpz_struct pointer which reflects the answer, set
 * gcState.frontier and return the answer.
 * If the answer fits in a fixnum, we return that, with the frontier
 * rolled back.
 * If the answer doesn't need all of the space allocated, we adjust
 * the sequence size and roll the frontier slightly back.
 */
objptr finiIntInfRes (GC_state s, __mpz_struct *res, size_t bytes) {
  GC_intInf bp;
  int size;

  // will be overwritten by __mpz_is_small
  objptr small_res;

  // ensure correct format of the structs
  __mpz_is_correct_fmt (res);

  if (DEBUG_INT_INF)
    fprintf (stderr, "finiIntInfRes ("FMTPTR", %"PRIuMAX")\n",
             (uintptr_t)res, (uintmax_t)bytes);
  if (DEBUG_INT_INF_DETAILED)
    fprintf (stderr, "res --> %s\n",
             mpz_get_str (NULL, 10, res));

  size = __mpz_intInf_lineup (res, &bp);

  // The mpz result is small - this sets the small_res pointer as well
  if (__mpz_is_small (bp, size, &small_res)) {
    return small_res;
  }

  return __mpz_form_final (s, bp, size, bytes);
}

/*
 * Unlike the above method, a sequence is passed in for storing the two results
 * in order to return them in a functional manner (rather than setting refs at
 * the program level)
 */
objptr finiIntInfRes_2 (GC_state s, __mpz_struct *l_res, __mpz_struct *r_res,
                        GC_objptr_sequence finals, size_t l_bytes, size_t r_bytes) {
  GC_intInf l_bp, r_bp;
  int l_size, r_size;
  objptr l_final, r_final;

  /*
   * Check that both of the returned results are formatted
   * correctly before continuing
   */
  __mpz_is_correct_fmt (l_res);
  __mpz_is_correct_fmt (r_res);

  if (DEBUG_INT_INF)
    fprintf (stderr, "finiIntInfRes_2 ("FMTPTR", "FMTPTR", %"PRIuMAX", %"PRIuMAX")\n",
             (uintptr_t)l_res, (uintptr_t)r_res, (uintmax_t)l_bytes, (uintmax_t)r_bytes);
  if (DEBUG_INT_INF_DETAILED)
    fprintf (stderr, "l_res --> %s\nr_res --> %s\n",
             mpz_get_str (NULL, 10, l_res), mpz_get_str (NULL, 10, r_res));

  // Line up the objects with the results, and return the resultant sizes
  // acquired from a possible sign flip if negative
  l_size = __mpz_intInf_lineup (l_res, &l_bp);
  r_size = __mpz_intInf_lineup (r_res, &r_bp);

  // finalize the left result in the heap if necessary
  unless (__mpz_is_small (l_bp, l_size, &l_final)) {
    if (DEBUG_INT_INF_DETAILED)
      fprintf (stderr, "Quotient isn't small\n");
    // set frontier forward the number of bytes used
    l_final = __mpz_form_final (s, l_bp, l_size, l_bytes);
  }

  // finalize the right result in the heap if necessary
  // roll the result back to the frontier if necessary
  unless (__mpz_is_small (r_bp, r_size, &r_final)) {
    if (DEBUG_INT_INF_DETAILED)
      fprintf (stderr, "Remainder isn't small\n");

    // Move the right result to the frontier if it isn't already there
    if ((GC_intInf)s->frontier != r_bp) {
      memmove ((void*)s->frontier,
               (const void*)(r_bp),
               r_bytes);
      // point the pointer at the frontier
      r_bp = (GC_intInf)(s->frontier);
      if (DEBUG_INT_INF_DETAILED)
        fprintf(stderr, "Remainder was shifted back to heap frontier\n");
    }
    // Set frontier forward the number of bytes used (after possible shift back)
    r_final = __mpz_form_final (s, r_bp, r_size, r_bytes);
  }

  // Result processing done - pack results into `finals` and return an objptr to it
  finals->objs[0] = l_final;
  finals->objs[1] = r_final;

  return pointerToObjptr ((pointer)&finals->objs, s->heap.start);
}

objptr IntInf_binop (GC_state s,
                     objptr lhs, objptr rhs, size_t bytes,
                     void(*binop)(__mpz_struct *resmpz,
                                  const __mpz_struct *lhsspace,
                                  const __mpz_struct *rhsspace)) {
  __mpz_struct lhsmpz, rhsmpz, resmpz;
  mp_limb_t lhsspace[LIMBS_PER_OBJPTR + 1], rhsspace[LIMBS_PER_OBJPTR + 1];

  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_binop ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  initIntInfRes (s, &resmpz, bytes);
  fillIntInfArg (s, lhs, &lhsmpz, lhsspace);
  fillIntInfArg (s, rhs, &rhsmpz, rhsspace);
  binop (&resmpz, &lhsmpz, &rhsmpz);
  return finiIntInfRes (s, &resmpz, bytes);
}

// the returned objptr is to a sequence containing the two results
objptr IntInf_binop_2 (GC_state s,
                       objptr lhs, objptr rhs,
                       size_t l_bytes, size_t r_bytes,
                       void(*binop)(__mpz_struct *l_res_mpz,
                                    __mpz_struct *r_res_mpz,
                                    const __mpz_struct *lhsspace,
                                    const __mpz_struct *rhsspace)) {

  __mpz_struct lhsmpz, rhsmpz, l_res_mpz, r_res_mpz;
  mp_limb_t lhsspace[LIMBS_PER_OBJPTR + 1], rhsspace[LIMBS_PER_OBJPTR + 1];
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_binop_2 ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)l_bytes, (uintmax_t)r_bytes);

  // get the sequence for storing the final results (will be allocated on the stack here)
  GC_objptr_sequence finals = initIntInfRes_2 (s, &l_res_mpz, &r_res_mpz, l_bytes, r_bytes);
  fillIntInfArg (s, lhs, &lhsmpz, lhsspace);
  fillIntInfArg (s, rhs, &rhsmpz, rhsspace);
  binop (&l_res_mpz, &r_res_mpz, &lhsmpz, &rhsmpz);

  return finiIntInfRes_2 (s, &l_res_mpz, &r_res_mpz, finals, l_bytes, r_bytes);
}

objptr IntInf_unop (GC_state s,
                    objptr arg, size_t bytes,
                    void(*unop)(__mpz_struct *resmpz,
                                const __mpz_struct *argspace)) {
  __mpz_struct argmpz, resmpz;
 mp_limb_t argspace[LIMBS_PER_OBJPTR + 1];

  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_unop ("FMTOBJPTR", %"PRIuMAX")\n",
             arg, (uintmax_t)bytes);

 initIntInfRes (s, &resmpz, bytes);
 fillIntInfArg (s, arg, &argmpz, argspace);
 unop (&resmpz, &argmpz);
 return finiIntInfRes (s, &resmpz, bytes);
}

objptr IntInf_shop (GC_state s,
                    objptr arg, Word32_t shift, size_t bytes,
                    void(*shop)(__mpz_struct *resmpz,
                                const __mpz_struct *argspace,
                                unsigned long shift))
{
  __mpz_struct argmpz, resmpz;
  mp_limb_t argspace[LIMBS_PER_OBJPTR + 1];

  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_shop ("FMTOBJPTR", %"PRIu32", %"PRIuMAX")\n",
             arg, shift, (uintmax_t)bytes);

  initIntInfRes (s, &resmpz, bytes);
  fillIntInfArg (s, arg, &argmpz, argspace);
  shop (&resmpz, &argmpz, (unsigned long)shift);
  return finiIntInfRes (s, &resmpz, bytes);
}

Int32_t IntInf_cmpop (GC_state s, objptr lhs, objptr rhs,
                      int(*cmpop)(const __mpz_struct *lhsspace,
                                  const __mpz_struct *rhsspace))
{
  __mpz_struct lhsmpz, rhsmpz;
  mp_limb_t lhsspace[LIMBS_PER_OBJPTR + 1], rhsspace[LIMBS_PER_OBJPTR + 1];
  int res;

  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_cmpop ("FMTOBJPTR", "FMTOBJPTR")\n",
             lhs, rhs);
  fillIntInfArg (s, lhs, &lhsmpz, lhsspace);
  fillIntInfArg (s, rhs, &rhsmpz, rhsspace);
  res = cmpop (&lhsmpz, &rhsmpz);
  if (res < 0) return -1;
  if (res > 0) return 1;
  return 0;
}

objptr IntInf_strop (GC_state s, objptr arg, Int32_t base, size_t bytes,
                     char*(*strop)(char *str,
                                   int base,
                                   const __mpz_struct *argspace))
{
  GC_string8 sp;
  __mpz_struct argmpz;
  mp_limb_t argspace[LIMBS_PER_OBJPTR + 1];
  char *str;
  size_t size;

  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_strop ("FMTOBJPTR", %"PRId32", %"PRIuMAX")\n",
             arg, base, (uintmax_t)bytes);
  assert (base == 2 || base == 8 || base == 10 || base == 16);
  fillIntInfArg (s, arg, &argmpz, argspace);
  assert (bytes <= (size_t)(s->limitPlusSlop - s->frontier));
  sp = (GC_string8)s->frontier;
  str = strop ((void*)&sp->obj, -base, &argmpz);
  assert (str == (char*)&sp->obj);
  size = strlen(str);
  if (sp->obj.chars[0] == '-')
    sp->obj.chars[0] = '~';
  setFrontier (s, (pointer)&sp->obj + size, bytes);
  sp->counter = (GC_sequenceCounter)0;
  sp->length = (GC_sequenceLength)size;
  sp->header = GC_STRING8_HEADER;
  return pointerToObjptr ((pointer)&sp->obj, s->heap.start);
}

/*
static GC_state intInfMemoryFuncsState;

static void * wrap_alloc_func(size_t size) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "alloc_func (size = %"PRIuMAX") = ", 
             (uintmax_t)size);
  void * res = (*alloc_func_ptr)(size);
  if (DEBUG_INT_INF)
    fprintf (stderr, FMTPTR"\n", (uintptr_t)res);
  return res;
}

static void * wrap_realloc_func(void *ptr, size_t old_size, size_t new_size) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "realloc_func (ptr = "FMTPTR", "
             "old_size = %"PRIuMAX", new_size = %"PRIuMAX") = ", 
             (uintptr_t)ptr, (uintmax_t)old_size, (uintmax_t)new_size);
  assert (! isPointerInHeap(intInfMemoryFuncsState, (pointer)ptr));
  void * res = (*realloc_func_ptr)(ptr, old_size, new_size);
  if (DEBUG_INT_INF)
    fprintf (stderr, FMTPTR"\n", (uintptr_t)res);
  return res;
}

static void wrap_free_func(void *ptr, size_t size) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "free_func (ptr = "FMTPTR", size = %"PRIuMAX")", 
             (uintptr_t)ptr, (uintmax_t)size);
  assert (! isPointerInHeap(intInfMemoryFuncsState, (pointer)ptr));
  (*free_func_ptr)(ptr, size);
  if (DEBUG_INT_INF)
    fprintf (stderr, "\n");
  return;
}

void initIntInf (GC_state s) {
  intInfMemoryFuncsState = s;
  mp_get_memory_functions (&alloc_func_ptr, &realloc_func_ptr, &free_func_ptr);
  mp_set_memory_functions (&wrap_alloc_func, &wrap_realloc_func, &wrap_free_func);
  return;
}
*/

void initIntInf (__attribute__ ((unused)) GC_state s) {
  return;
}
