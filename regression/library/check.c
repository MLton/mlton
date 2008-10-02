#include <assert.h>

#include "check.h"
#include "m5.h"
#include "m4.h"
#define DYNAMIC_LINK_M3
#include "m3.h"

extern EXTERNAL void* libm3cSymPublic;
extern EXTERNAL void* libm3cFnPublic(void);
extern EXTERNAL void* libm4cSymPublic;
extern EXTERNAL void* libm4cFnPublic(void);

extern PUBLIC void* libm5cSymPublic;
extern PUBLIC void* libm5cFnPublic(void);

PRIVATE void* checkcSymPrivate = 0;
PUBLIC  void* checkcSymPublic  = 0;

PRIVATE void* checkcFnPrivate(void) {
  return &checkcSymPrivate;
}

PUBLIC void* checkcFnPublic(void) {
  return &checkcSymPublic;
}

PRIVATE void checkconfirmC(void) {
  assert (&checksmlFnPrivate == checksmlSymPrivate);
  assert (&checksmlFnPublic  == checksmlSymPublic);
  assert (&checkcFnPrivate   == checkcSymPrivate);
  assert (&checkcFnPublic    == checkcSymPublic);
  
  assert (checksmlFnPrivate() == &checksmlSymPrivate);
  assert (checksmlFnPublic()  == &checksmlSymPublic);
  
  /* Check libm3 */
  assert (&libm3smlFnPublic  == libm3smlSymPublic);
  assert (&libm3cFnPublic    == libm3cSymPublic);
  assert (libm3smlFnPublic() == &libm3smlSymPublic);
  assert (libm3cFnPublic()   == &libm3cSymPublic);
  
  /* Check libm4 */
  assert (&libm4smlFnPublic  == libm4smlSymPublic);
  assert (&libm4cFnPublic    == libm4cSymPublic);
  assert (libm4smlFnPublic() == &libm4smlSymPublic);
  assert (libm4cFnPublic()   == &libm4cSymPublic);
  
  /* Check libm5 */
  assert (&libm5smlFnPublic  == libm5smlSymPublic);
  assert (&libm5cFnPublic    == libm5cSymPublic);
  assert (libm5smlFnPublic() == &libm5smlSymPublic);
  assert (libm5cFnPublic()   == &libm5cSymPublic);
}
