#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_connect(Int s, Char *addr, Int addrlen) {
	return connect(s, (struct sockaddr*)addr, (socklen_t)addrlen);
}
