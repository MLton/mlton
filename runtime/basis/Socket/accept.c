#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

static struct sockaddr_in sockaddr_in;

Word Socket_Addr_address() {
	return ntohl(sockaddr_in.sin_addr.s_addr);
}

Int Socket_Addr_port() {
	return ntohs(sockaddr_in.sin_port);
}

/* sl should have been created by Socket_listen.
 * Socket_accept returns the new file descriptor for the incoming connection.
 */
Int Socket_accept(Int sl) {
	int	len;
	int	fd;

	len = sizeof(sockaddr_in);
	fd = accept(sl, (struct sockaddr *)&sockaddr_in, &len);
	assert(sockaddr_in.sin_family == AF_INET);
	return fd;
}
