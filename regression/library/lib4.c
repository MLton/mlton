#include <assert.h>

#define PART_OF_LIBRARY_LIB4
#include "lib4.h"
#include "lib3.h"
#include "lib2.h"
#include "lib1.h"

extern EXTERNAL void* lib1cSymPublic;
extern EXTERNAL void* lib1cFnPublic(void);
extern EXTERNAL void* lib2cSymPublic;
extern EXTERNAL void* lib2cFnPublic(void);

extern PUBLIC void* lib3cSymPublic;
extern PUBLIC void* lib3cFnPublic(void);

PRIVATE void* lib4cSymPrivate = 0;
PUBLIC  void* lib4cSymPublic  = 0;

PRIVATE void* lib4cFnPrivate(void) {
  return &lib4cSymPrivate;
}

PUBLIC void* lib4cFnPublic(void) {
  return &lib4cSymPublic;
}

PRIVATE void lib4confirmC(void) {
  assert (&lib4smlFnPrivate == lib4smlSymPrivate);
  assert (&lib4smlFnPublic  == lib4smlSymPublic);
  assert (&lib4cFnPrivate   == lib4cSymPrivate);
  assert (&lib4cFnPublic    == lib4cSymPublic);
  
  assert (lib4smlFnPrivate() == &lib4smlSymPrivate);
  assert (lib4smlFnPublic()  == &lib4smlSymPublic);
  
  /* Check lib1 */
  assert (&lib1smlFnPublic  == lib1smlSymPublic);
  assert (&lib1cFnPublic    == lib1cSymPublic);
  assert (lib1smlFnPublic() == &lib1smlSymPublic);
  assert (lib1cFnPublic()   == &lib1cSymPublic);
  
  /* Check lib2 */
  assert (&lib2smlFnPublic  == lib2smlSymPublic);
  assert (&lib2cFnPublic    == lib2cSymPublic);
  assert (lib2smlFnPublic() == &lib2smlSymPublic);
  assert (lib2cFnPublic()   == &lib2cSymPublic);
  
  /* Check lib3 */
  assert (&lib3smlFnPublic  == lib3smlSymPublic);
  assert (&lib3cFnPublic    == lib3cSymPublic);
  assert (lib3smlFnPublic() == &lib3smlSymPublic);
  assert (lib3cFnPublic()   == &lib3cSymPublic);
}
