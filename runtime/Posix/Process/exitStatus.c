#include "platform.h"

Int Posix_Process_exitStatus (Status s) {
	return WEXITSTATUS (s);
}
