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
		reventss[i] = ufds[i].events;
	}
	return res;
}

/* Modified from SML/NJ sources by Matthew Fluet 2003-06-06 */
/* poll.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * The run-time code for OS.IO.poll.  Note that this implementation should
 * satisfy the following two properties:
 *
 *   1) the list of return items should be in the same order as the
 *	corresponding list of arguments.
 *
 *   2) return items should contain no more information than was queried for
 *	(this matters when the same descriptor is covered by multiple items).
 */
/*
#include <stdlib.h>
#include <sys/poll.h>
#include <sys/select.h>
#include "mlton-basis.h"
#include "mlton-posix.h"

Int OS_IO_poll(Fd *fds, Word *eventss, Int n, Int timeout, Word *reventss) {
  fd_set	rset, wset, eset;
  fd_set	*rfds, *wfds, *efds;
  struct timeval timeoutZ;
  struct timeval *timeoutZZ;
  int		maxFD, fd, flag, resFlag;
  int 		i, res;

  rfds = wfds = efds = NULL;
  maxFD = 0;
  for (i = 0; i < n; i++) {
    fd	= fds[i];
    flag = eventss[i];
    if ((flag & POLLIN) != 0) {
      if (rfds == NULL) {
	rfds = &rset;
	FD_ZERO(rfds);
      }
      FD_SET (fd, rfds);
    }
    if ((flag & POLLOUT) != 0) {
      if (wfds == NULL) {
	wfds = &wset;
	FD_ZERO(wfds);
      }
      FD_SET (fd, wfds);
    }
    if ((flag & POLLPRI) != 0) {
      if (efds == NULL) {
	efds = &eset;
	FD_ZERO(efds);
      }
      FD_SET (fd, efds);
    }
    if (fd > maxFD) maxFD = fd;
  }
  if (timeout < 0)
    timeoutZZ = NULL;
  else {
    timeoutZZ = &timeoutZ;
    timeoutZ.tv_sec = timeout / 1000;
    timeoutZ.tv_usec = (timeout % 1000) * 1000;
  }
  res = select (maxFD+1, rfds, wfds, efds, timeoutZZ);
  if (res < 0)
    return res;
  else {
    for (i = 0; i < n; i++) {
      fd = fds[i];
      flag = eventss[i];
      resFlag = 0;
      if (((flag & POLLIN) != 0) && FD_ISSET(fd, rfds))
	resFlag |= POLLIN;
      if (((flag & POLLOUT) != 0) && FD_ISSET(fd, wfds))
	resFlag |= POLLOUT;
      if (((flag & POLLPRI) != 0) && FD_ISSET(fd, efds))
	resFlag |= POLLPRI;
      reventss[i] = resFlag;
    }
    return res;
  }
}
*/
