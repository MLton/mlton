#include "platform.h"

Bool Posix_Process_ifSignaled (Status s) {
	return WIFSIGNALED (s);
}
