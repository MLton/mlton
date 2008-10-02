#include <assert.h>

#define PART_OF_M2
#include "m2.h"
#define STATIC_LINK_M1
#include "m1.h"

extern PUBLIC void* libm1cSymPublic;
extern PUBLIC void* libm1cFnPublic(void);

PRIVATE void* libm2cSymPrivate = 0;
PUBLIC  void* libm2cSymPublic  = 0;

PRIVATE void* libm2cFnPrivate(void) {
  return &libm2cSymPrivate;
}

PUBLIC void* libm2cFnPublic(void) {
  return &libm2cSymPublic;
}

PRIVATE void libm2confirmC(void) {
  assert (&libm2smlFnPrivate == libm2smlSymPrivate);
  assert (&libm2smlFnPublic  == libm2smlSymPublic);
  assert (&libm2cFnPrivate   == libm2cSymPrivate);
  assert (&libm2cFnPublic    == libm2cSymPublic);
  
  assert (libm2smlFnPrivate() == &libm2smlSymPrivate);
  assert (libm2smlFnPublic()  == &libm2smlSymPublic);
  
  /* Check libm1 */
  assert (&libm1smlFnPublic  == libm1smlSymPublic);
  assert (&libm1cFnPublic    == libm1cSymPublic);
  assert (libm1smlFnPublic() == &libm1smlSymPublic);
  assert (libm1cFnPublic()   == &libm1cSymPublic);
}
