#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_sendTo(Int s, Char *msg, Int start, Int len, Word flags,
                  Char* addr, Int addrlen) {
	return sendto(s, (void*)((char *)msg + start), (size_t)len, flags,
                      (struct sockaddr*)addr, (socklen_t)addrlen);
}
