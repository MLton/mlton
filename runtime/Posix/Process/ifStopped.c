#include "platform.h"

Bool Posix_Process_ifStopped (Status s) {
	return WIFSTOPPED (s);
}
