#include <unistd.h>
#include <fcntl.h>
#include "mlton-posix.h"

static struct flock flock;

Int Posix_IO_FLock_fcntl(Fd f, Int cmd) {
	return fcntl(f, cmd, (int)&flock);
}

Int Posix_IO_FLock_type() {
	return flock.l_type;
}

Int Posix_IO_FLock_whence() {
	return flock.l_whence;
}

Int Posix_IO_FLock_start() {
	return flock.l_start;
}

Int Posix_IO_FLock_len() {
	return flock.l_len;
}

Int Posix_IO_FLock_pid() {
	return flock.l_pid;
}

void Posix_IO_FLock_setType(Int x) {
	flock.l_type = x;
}

void Posix_IO_FLock_setWhence(Int x) {
	flock.l_whence = x;
} 

void Posix_IO_FLock_setStart(Int x) {
	flock.l_start = x;
} 

void Posix_IO_FLock_setLen(Int x) {
	flock.l_len = x;
} 

void Posix_IO_FLock_setPid(Int x) {
	flock.l_pid = x;
} 
