#include <unistd.h>
#include "mlton-posix.h"

int Posix_Process_sleep(int i) {
	return sleep(i);
}
