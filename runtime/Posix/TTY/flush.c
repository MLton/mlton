#include <termios.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_TTY_flush(Fd f, Int i) {
	return tcflush(f, i);
}
