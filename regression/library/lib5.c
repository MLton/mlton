#include <assert.h>

#define PART_OF_LIBRARY_LIB5
#include "lib5.h"
#include "lib4.h"
#include "lib3.h"

extern EXTERNAL void* lib3cSymPublic;
extern EXTERNAL void* lib3cFnPublic(void);
extern EXTERNAL void* lib4cSymPublic;
extern EXTERNAL void* lib4cFnPublic(void);

PRIVATE void* lib5cSymPrivate = 0;
PUBLIC  void* lib5cSymPublic  = 0;

PRIVATE void* lib5cFnPrivate(void) {
  return &lib5cSymPrivate;
}

PUBLIC void* lib5cFnPublic(void) {
  return &lib5cSymPublic;
}

PRIVATE void lib5confirmC(void) {
  assert (&lib5smlFnPrivate == lib5smlSymPrivate);
  assert (&lib5smlFnPublic  == lib5smlSymPublic);
  assert (&lib5cFnPrivate   == lib5cSymPrivate);
  assert (&lib5cFnPublic    == lib5cSymPublic);
  
  assert (lib5smlFnPrivate() == &lib5smlSymPrivate);
  assert (lib5smlFnPublic()  == &lib5smlSymPublic);
  
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
}
