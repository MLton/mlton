#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_Ctl_getSockOpt(Int s, Int level, Int optname, Char *optval, Int *optlen) {
	return getsockopt(s, level, optname, (void*)optval, (socklen_t*)optlen);
}

Int Socket_Ctl_setSockOpt(Int s, Int level, Int optname, Char *optval, Int optlen) {
	return setsockopt(s, level, optname, (void*)optval, (socklen_t)optlen);
}

Int Socket_Ctl_getsetIOCtl(Int s, Int request, Char* argp) {
	return ioctl(s, request, argp);
}

Int Socket_Ctl_getPeerName(Int s, Char *name, Int *namelen) {
	return getpeername(s, (struct sockaddr*)name, (socklen_t*)namelen);
}

Int Socket_Ctl_getSockName(Int s, Char *name, Int *namelen) {
	return getsockname(s, (struct sockaddr*)name, (socklen_t*)namelen);
}
