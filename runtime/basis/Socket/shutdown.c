#if (defined (__FreeBSD__))
#include <sys/types.h>
#endif
#include <sys/socket.h>
#include "mlton-basis.h"

Int Socket_shutdown(Int fd, Int how) {
	return shutdown(fd, how);
}
