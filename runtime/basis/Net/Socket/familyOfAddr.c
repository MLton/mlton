#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_familyOfAddr(Char *addr) {
	return ((struct sockaddr*)addr)->sa_family;
}
