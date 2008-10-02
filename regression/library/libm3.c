#include <assert.h>

#define PART_OF_M3
#include "m3.h"
#include "m2.h"
#define DYNAMIC_LINK_M1
#include "m1.h"

extern EXTERNAL void* libm1cSymPublic;
extern EXTERNAL void* libm1cFnPublic(void);
extern EXTERNAL void* libm2cSymPublic;
extern EXTERNAL void* libm2cFnPublic(void);

PRIVATE void* libm3cSymPrivate = 0;
PUBLIC  void* libm3cSymPublic  = 0;

PRIVATE void* libm3cFnPrivate(void) {
  return &libm3cSymPrivate;
}

PUBLIC void* libm3cFnPublic(void) {
  return &libm3cSymPublic;
}

PRIVATE void libm3confirmC(void) {
  assert (&libm3smlFnPrivate == libm3smlSymPrivate);
  assert (&libm3smlFnPublic  == libm3smlSymPublic);
  assert (&libm3cFnPrivate   == libm3cSymPrivate);
  assert (&libm3cFnPublic    == libm3cSymPublic);
  
  assert (libm3smlFnPrivate() == &libm3smlSymPrivate);
  assert (libm3smlFnPublic()  == &libm3smlSymPublic);
  
  /* Check libm1 */
  assert (&libm1smlFnPublic  == libm1smlSymPublic);
  assert (&libm1cFnPublic    == libm1cSymPublic);
  assert (libm1smlFnPublic() == &libm1smlSymPublic);
  assert (libm1cFnPublic()   == &libm1cSymPublic);
  
  /* Check libm2 */
  assert (&libm2smlFnPublic  == libm2smlSymPublic);
  assert (&libm2cFnPublic    == libm2cSymPublic);
  assert (libm2smlFnPublic() == &libm2smlSymPublic);
  assert (libm2cFnPublic()   == &libm2cSymPublic);
}
