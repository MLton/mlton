#include <assert.h>

#define PART_OF_M1
#include "m1.h"

PRIVATE void* libm1cSymPrivate = 0;
PUBLIC  void* libm1cSymPublic  = 0;

PRIVATE void* libm1cFnPrivate(void) {
  return &libm1cSymPrivate;
}

PUBLIC void* libm1cFnPublic(void) {
  return &libm1cSymPublic;
}

PRIVATE void libm1confirmC(void) {
  assert (&libm1smlFnPrivate == libm1smlSymPrivate);
  assert (&libm1smlFnPublic  == libm1smlSymPublic);
  assert (&libm1cFnPrivate   == libm1cSymPrivate);
  assert (&libm1cFnPublic    == libm1cSymPublic);
  
  assert (libm1smlFnPrivate() == &libm1smlSymPrivate);
  assert (libm1smlFnPublic()  == &libm1smlSymPublic);
}
