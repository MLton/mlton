#include <termios.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_TTY_getpgrp(Fd f) {
	return tcgetpgrp(f);
}
