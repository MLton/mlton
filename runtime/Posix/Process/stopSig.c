#include "platform.h"

Signal Posix_Process_stopSig (Status s) {
	return WSTOPSIG (s);
}
