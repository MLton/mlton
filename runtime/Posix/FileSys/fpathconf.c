#include <unistd.h>
#include "mlton-posix.h"

Int Posix_FileSys_fpathconf(Fd f, Int n) {
	return fpathconf(f, n);
}
