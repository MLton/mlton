#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_connect(Pointer host, Int port) {
	int			res;
	struct sockaddr_in	addr;
	struct hostent		*hp;

	assert(port > 0);
	hp = gethostbyname((char*)host);
	if (hp == NULL) return -1;
	res = socket(AF_INET, SOCK_STREAM, 0);
	if (res < 0) return -1;
	memset((char*)&addr, 0, sizeof(addr));
	addr.sin_family = hp->h_addrtype;
	assert(hp->h_length <= sizeof(addr.sin_addr));
	memcpy((char *)&addr.sin_addr, hp->h_addr, hp->h_length);
	addr.sin_port = htons(port);
	if (connect(res, (struct sockaddr *)&addr, sizeof(addr)) < 0) return -1;
	return res;
}
