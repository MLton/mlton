#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Net_htonl(Int i) {
	return htonl(i);
}

Int Net_ntohl(Int i) {
	return ntohl(i);
}

Int Net_htons(Int i) {
	return htons(i);
}

Int Net_ntohs(Int i) {
	return ntohs(i);
}
