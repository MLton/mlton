#include <termios.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_TTY_setpgrp(Fd f, Pid p) {
	return tcsetpgrp(f, p);
}
