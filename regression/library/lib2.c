#include <assert.h>

#define PART_OF_LIBRARY_LIB2
#include "lib2.h"
#include "lib1.h"

extern PUBLIC void* lib1cSymPublic;
extern PUBLIC void* lib1cFnPublic(void);

PRIVATE void* lib2cSymPrivate = 0;
PUBLIC  void* lib2cSymPublic  = 0;

PRIVATE void* lib2cFnPrivate(void) {
  return &lib2cSymPrivate;
}

PUBLIC void* lib2cFnPublic(void) {
  return &lib2cSymPublic;
}

PRIVATE void lib2confirmC(void) {
  assert (&lib2smlFnPrivate == lib2smlSymPrivate);
  assert (&lib2smlFnPublic  == lib2smlSymPublic);
  assert (&lib2cFnPrivate   == lib2cSymPrivate);
  assert (&lib2cFnPublic    == lib2cSymPublic);
  
  assert (lib2smlFnPrivate() == &lib2smlSymPrivate);
  assert (lib2smlFnPublic()  == &lib2smlSymPublic);
  
  /* Check lib1 */
  assert (&lib1smlFnPublic  == lib1smlSymPublic);
  assert (&lib1cFnPublic    == lib1cSymPublic);
  assert (lib1smlFnPublic() == &lib1smlSymPublic);
  assert (lib1cFnPublic()   == &lib1cSymPublic);
}
