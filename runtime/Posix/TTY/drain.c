#include <termios.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_TTY_drain(Fd f) {
	return tcdrain(f);
}
