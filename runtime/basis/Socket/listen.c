#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include "mlton-basis.h"
#include "my-lib.h"

enum {
	LISTENQ = 5,
};

/* This sets resultSocketp, and it sets port if *portp == 0. */
Int Socket_listen(Pointer portp, Pointer resultSocketp) {
	struct sockaddr_in	addr;
	int 			sl, len;
	int *port;
	int *resultSocket;
	int	yes;

	yes = TRUE;
	port = (int*)portp;
	resultSocket = (int*)resultSocketp;
	sl = socket(AF_INET, SOCK_STREAM, 0);
	if (sl < 0) return -1;
	unless (0 == setsockopt(sl, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)))
		die("Socket_listen setsockopt SO_REUSEADDR failed");
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(*port);
	if (bind(sl, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		close(sl);
		return -1;
	}
	if (listen(sl, LISTENQ) < 0) {
		close(sl);
		return -1;
	}
	if (*port == 0) {
		len = sizeof(addr);
		if (getsockname(sl, (struct sockaddr *)&addr, &len) < 0) {
			close(sl);
			return -1;
		}
		assert(addr.sin_family == AF_INET);
		*port = htons(addr.sin_port);
	}
	*resultSocket = sl;
	return 0;
}
