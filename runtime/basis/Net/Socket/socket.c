#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int GenericSock_socket(Int domain, Int type, Int protocol) {
	return socket(domain, type, protocol);
}
