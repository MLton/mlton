#include "gc.h"
#include "mlton-basis.h"

Int Array_numElements(Pointer p) {
	return GC_arrayNumElements(p);
}
