#include <sys/poll.h>
#include "mlton-basis.h"
#include "mlton-posix.h"

Int OS_IO_poll(Fd *fds, Word *eventss, Int n, Int timeout, Word *reventss) {
	int i, res;
	struct pollfd ufds[n];

	for (i = 0; i < n; i++) {
		ufds[i].fd = fds[i];
		ufds[i].events = eventss[i];
	}
	res = poll(ufds, n, timeout);
	for (i = 0; i < n; i++) {
		reventss[i] = ufds[i].revents;
	}
	return res;
}
