#include <stdio.h>
#include "mlton-posix.h"
#include "my-lib.h"

Cstring Posix_ProcEnv_ctermid() {
	return (Cstring)ctermid(NULL);
}
