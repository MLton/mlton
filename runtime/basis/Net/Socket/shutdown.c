#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_shutdown(Int s, Int how) {
	return shutdown(s, how);
}
