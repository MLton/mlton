#include "platform.h"

Int Posix_Process_pause () {
	return pause ();
}
