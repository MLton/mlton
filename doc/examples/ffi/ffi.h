/* ffi.h */

#include "libmlton.h"

#define BOOL0 0
#define BOOL1 1
#define INT0 -1
#define INT1 0
#define INT2 1
#define REAL0 -1.234
#define REAL1 1.234
#define STRING0 "hello there\nhow are you\n"
#define WORD0 0x0
#define WORD1 0xFFFFFFFF

#define FFI_SIZE 10

extern Int FFI_INT;

/* ffi is a silly function.  It sums the elements ds, stores the
 * result in p, adds i to each element of ds, and returns 'c'.
 */
Char ffi (Pointer ds, Pointer p, Int n);

