#include <sys/socket.h>
#include "mlton-basis.h"

Int Socket_shutdown(Int fd, Int how) {
	return shutdown(fd, how);
}
