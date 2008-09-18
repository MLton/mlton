#include <assert.h>

#define PART_OF_LIBRARY_LIB3
#include "lib3.h"
#include "lib2.h"
#include "lib1.h"

extern EXTERNAL void* lib1cSymPublic;
extern EXTERNAL void* lib1cFnPublic(void);
extern EXTERNAL void* lib2cSymPublic;
extern EXTERNAL void* lib2cFnPublic(void);

PRIVATE void* lib3cSymPrivate = 0;
PUBLIC  void* lib3cSymPublic  = 0;

PRIVATE void* lib3cFnPrivate(void) {
  return &lib3cSymPrivate;
}

PUBLIC void* lib3cFnPublic(void) {
  return &lib3cSymPublic;
}

PRIVATE void lib3confirmC(void) {
  assert (&lib3smlFnPrivate == lib3smlSymPrivate);
  assert (&lib3smlFnPublic  == lib3smlSymPublic);
  assert (&lib3cFnPrivate   == lib3cSymPrivate);
  assert (&lib3cFnPublic    == lib3cSymPublic);
  
  assert (lib3smlFnPrivate() == &lib3smlSymPrivate);
  assert (lib3smlFnPublic()  == &lib3smlSymPublic);
  
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
}
