#include "platform.h"

Bool Posix_Process_ifExited (Status s) {
	return WIFEXITED (s);
}
