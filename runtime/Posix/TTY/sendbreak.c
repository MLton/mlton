#include <termios.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_TTY_sendbreak(Fd f, Int i) {
	return tcsendbreak(f, i);
}
