#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_bind(Int s, Char *addr, Int addrlen) {
	return bind(s, (struct sockaddr*)addr, (socklen_t)addrlen);
}
