#include "platform.h"

Int Array_numElements (Pointer p) {
        return GC_getArrayLength (p);
}
