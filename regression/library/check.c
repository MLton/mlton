#include <assert.h>

#include "check.h"
#include "lib5.h"
#include "lib4.h"
#include "lib3.h"

extern EXTERNAL void* lib3cSymPublic;
extern EXTERNAL void* lib3cFnPublic(void);
extern EXTERNAL void* lib4cSymPublic;
extern EXTERNAL void* lib4cFnPublic(void);

extern PUBLIC void* lib5cSymPublic;
extern PUBLIC void* lib5cFnPublic(void);

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
  
  /* Check lib3 */
  assert (&lib3smlFnPublic  == lib3smlSymPublic);
  assert (&lib3cFnPublic    == lib3cSymPublic);
  assert (lib3smlFnPublic() == &lib3smlSymPublic);
  assert (lib3cFnPublic()   == &lib3cSymPublic);
  
  /* Check lib4 */
  assert (&lib4smlFnPublic  == lib4smlSymPublic);
  assert (&lib4cFnPublic    == lib4cSymPublic);
  assert (lib4smlFnPublic() == &lib4smlSymPublic);
  assert (lib4cFnPublic()   == &lib4cSymPublic);
  
  /* Check lib5 */
  assert (&lib5smlFnPublic  == lib5smlSymPublic);
  assert (&lib5cFnPublic    == lib5cSymPublic);
  assert (lib5smlFnPublic() == &lib5smlSymPublic);
  assert (lib5cFnPublic()   == &lib5cSymPublic);
}
