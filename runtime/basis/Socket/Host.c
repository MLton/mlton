#include <netdb.h>
#if (defined (__FreeBSD__))
#include <sys/types.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include "mlton-basis.h"
#include "my-lib.h"

static struct hostent *host;

Cstring Socket_Host_name() {
	return (Cstring)host->h_name;
}

Int Socket_Host_getByAddress(Word addr) {
	struct in_addr a;

	a.s_addr = htonl(addr);
	host = gethostbyaddr((void*)&a, sizeof(a), AF_INET);
	return (host != NULL and host->h_name != NULL);
}

Int Socket_Host_getByName(Cstring name) {
	host = gethostbyname((char*)name);
	return (host != NULL and host->h_name != NULL);
}
