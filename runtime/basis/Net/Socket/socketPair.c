#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_socketPair(Int domain, Int type, Int protocol, Int sv[2]) {
	return socketpair(domain, type, protocol, (int*)sv);
}
