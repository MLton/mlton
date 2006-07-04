#include "platform.h"

/* All of the Real{32,64}_nextAfter{Down,Up} functions work by converting the
 * real to a word of equivalent size and doing an increment or decrement on the
 * word.  This works because the SML Basis Library code that calls these
 * functions handles all the special cases (nans and infs).  Also, because of
 * the way IEEE floating point numbers are represented, word {de,in}crement
 * automatically does the right thing at the boundary between normals and
 * denormals.  Also, convienently, maxFinite+1 = posInf.
 */

typedef union {
  Real32_t r;
  Word32_t w;
} rw32;

Real32_t Real32_nextAfterDown (Real32_t r) {
  rw32 rw;
  rw.r = r;
  rw.w--;
  return rw.r;
}

Real32_t Real32_nextAfterUp (Real32_t r) {
  rw32 rw;
  rw.r = r;
  rw.w++;
  return rw.r;
}

typedef union {
  Real64_t r;
  Word64_t w;
} rw64;

Real64_t Real64_nextAfterDown (Real64_t r) {
  rw64 rw;
  rw.r = r;
  rw.w--;
  return rw.r;
}

Real64_t Real64_nextAfterUp (Real64_t r) {
  rw64 rw;
  rw.r = r;
  rw.w++;
  return rw.r;
}
