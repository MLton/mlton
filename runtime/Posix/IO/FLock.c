#include <unistd.h>
#include <fcntl.h>
#include "mlton-posix.h"

static struct flock s_flock;

Int Posix_IO_FLock_fcntl(Fd f, Int cmd) {
	return fcntl(f, cmd, (int)&s_flock);
}

Int Posix_IO_FLock_type() {
	return s_flock.l_type;
}

Int Posix_IO_FLock_whence() {
	return s_flock.l_whence;
}

Int Posix_IO_FLock_start() {
	return s_flock.l_start;
}

Int Posix_IO_FLock_len() {
	return s_flock.l_len;
}

Int Posix_IO_FLock_pid() {
	return s_flock.l_pid;
}

void Posix_IO_FLock_setType(Int x) {
	s_flock.l_type = x;
}

void Posix_IO_FLock_setWhence(Int x) {
	s_flock.l_whence = x;
} 

void Posix_IO_FLock_setStart(Int x) {
	s_flock.l_start = x;
} 

void Posix_IO_FLock_setLen(Int x) {
	s_flock.l_len = x;
} 

void Posix_IO_FLock_setPid(Int x) {
	s_flock.l_pid = x;
} 
