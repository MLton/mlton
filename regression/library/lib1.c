#include <assert.h>

#define PART_OF_LIBRARY_LIB1
#include "lib1.h"

PRIVATE void* lib1cSymPrivate = 0;
PUBLIC  void* lib1cSymPublic  = 0;

PRIVATE void* lib1cFnPrivate(void) {
  return &lib1cSymPrivate;
}

PUBLIC void* lib1cFnPublic(void) {
  return &lib1cSymPublic;
}

PRIVATE void lib1confirmC(void) {
  assert (&lib1smlFnPrivate == lib1smlSymPrivate);
  assert (&lib1smlFnPublic  == lib1smlSymPublic);
  assert (&lib1cFnPrivate   == lib1cSymPrivate);
  assert (&lib1cFnPublic    == lib1cSymPublic);
  
  assert (lib1smlFnPrivate() == &lib1smlSymPrivate);
  assert (lib1smlFnPublic()  == &lib1smlSymPublic);
}
