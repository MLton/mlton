#define _ISOC99_SOURCE

#include "platform.h"

Int Array_numElements (Pointer p) {
	return GC_arrayNumElements (p);
}
