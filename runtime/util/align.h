/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool isAligned (size_t a, size_t b) {
  return 0 == a % b;
}

static inline bool isAlignedMax (uintmax_t a, uintmax_t b) {
  return 0 == a % b;
}

static inline size_t alignDown (size_t a, size_t b) {
  assert (b >= 1);
  a -= a % b;
  assert (isAligned (a, b));
  return a;
}

static inline uintmax_t alignMaxDown (uintmax_t a, uintmax_t b) {
  assert (b >= 1);
  a -= a % b;
  assert (isAlignedMax (a, b));
  return a;
}

static inline size_t align (size_t a, size_t b) {
  assert (b >= 1);
  a += b - 1;
  a -= a % b;
  assert (isAligned (a, b));
  return a;       
}

static inline uintmax_t alignMax (uintmax_t a, uintmax_t b) {
  assert (b >= 1);
  a += b - 1;
  a -= a % b;
  assert (isAligned (a, b));
  return a;       
}
