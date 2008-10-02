#include <assert.h>

#define PART_OF_M4
#include "m4.h"
#define STATIC_LINK_M3
#include "m3.h"
#include "m2.h"
#define DYNAMIC_LINK_M1
#include "m1.h"

extern EXTERNAL void* libm1cSymPublic;
extern EXTERNAL void* libm1cFnPublic(void);
extern EXTERNAL void* libm2cSymPublic;
extern EXTERNAL void* libm2cFnPublic(void);

extern PUBLIC void* libm3cSymPublic;
extern PUBLIC void* libm3cFnPublic(void);

PRIVATE void* libm4cSymPrivate = 0;
PUBLIC  void* libm4cSymPublic  = 0;

PRIVATE void* libm4cFnPrivate(void) {
  return &libm4cSymPrivate;
}

PUBLIC void* libm4cFnPublic(void) {
  return &libm4cSymPublic;
}

PRIVATE void libm4confirmC(void) {
  assert (&libm4smlFnPrivate == libm4smlSymPrivate);
  assert (&libm4smlFnPublic  == libm4smlSymPublic);
  assert (&libm4cFnPrivate   == libm4cSymPrivate);
  assert (&libm4cFnPublic    == libm4cSymPublic);
  
  assert (libm4smlFnPrivate() == &libm4smlSymPrivate);
  assert (libm4smlFnPublic()  == &libm4smlSymPublic);
  
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
  
  /* Check libm3 */
  assert (&libm3smlFnPublic  == libm3smlSymPublic);
  assert (&libm3cFnPublic    == libm3cSymPublic);
  assert (libm3smlFnPublic() == &libm3smlSymPublic);
  assert (libm3cFnPublic()   == &libm3cSymPublic);
}
