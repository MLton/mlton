#include <stdio.h>
#include "mlton-basis.h"

Cstring OS_FileSys_tmpnam() {
	return (Cstring)tmpnam(NULL);
}
