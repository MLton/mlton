#include "platform.h"

Signal Posix_Process_termSig(Status s) {
	return WTERMSIG(s);
}
