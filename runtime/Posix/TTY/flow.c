#include <termios.h>
#include <unistd.h>
#include "mlton-posix.h"

Int Posix_TTY_flow(Fd f, Int i) {
	return tcflow(f, i);
}
