#include "platform.h"

Int Posix_Process_exitStatus (Status s) {
	int i;

	i = s;
	return WEXITSTATUS (i);
}
