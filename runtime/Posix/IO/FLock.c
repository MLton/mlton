#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

static struct flock s_flock;

Int Posix_IO_FLock_fcntl (Fd f, Int cmd) {
	return fcntl (f, cmd, (int)&s_flock);
}

Int Posix_IO_FLock_type () {
	return s_flock.l_type;
}

Int Posix_IO_FLock_whence () {
	return s_flock.l_whence;
}

Position Posix_IO_FLock_start () {
	return s_flock.l_start;
}

Position Posix_IO_FLock_len () {
	return s_flock.l_len;
}

Int Posix_IO_FLock_pid () {
	return s_flock.l_pid;
}

void Posix_IO_FLock_setType (Int x) {
	s_flock.l_type = x;
}

void Posix_IO_FLock_setWhence (Int x) {
	s_flock.l_whence = x;
} 

void Posix_IO_FLock_setStart (Position x) {
	s_flock.l_start = x;
} 

void Posix_IO_FLock_setLen (Position x) {
	s_flock.l_len = x;
} 

void Posix_IO_FLock_setPid (Int x) {
	s_flock.l_pid = x;
} 
