#include <assert.h>

#define PART_OF_M5
#include "m5.h"
#include "m4.h"
#define DYNAMIC_LINK_M3
#include "m3.h"

extern EXTERNAL void* libm3cSymPublic;
extern EXTERNAL void* libm3cFnPublic(void);
extern EXTERNAL void* libm4cSymPublic;
extern EXTERNAL void* libm4cFnPublic(void);

PRIVATE void* libm5cSymPrivate = 0;
PUBLIC  void* libm5cSymPublic  = 0;

PRIVATE void* libm5cFnPrivate(void) {
  return &libm5cSymPrivate;
}

PUBLIC void* libm5cFnPublic(void) {
  return &libm5cSymPublic;
}

PRIVATE void libm5confirmC(void) {
  assert (&libm5smlFnPrivate == libm5smlSymPrivate);
  assert (&libm5smlFnPublic  == libm5smlSymPublic);
  assert (&libm5cFnPrivate   == libm5cSymPrivate);
  assert (&libm5cFnPublic    == libm5cSymPublic);
  
  assert (libm5smlFnPrivate() == &libm5smlSymPrivate);
  assert (libm5smlFnPublic()  == &libm5smlSymPublic);
  
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
}
