#include "platform.h"

Bool Posix_Process_ifStopped (Status s) {
	int i;

	i = s;
	return WIFSTOPPED (i);
}
