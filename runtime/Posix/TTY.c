#include "platform.h"

static struct termios termios;

Flag Posix_TTY_Termios_iflag () {
	return termios.c_iflag;
}

Flag Posix_TTY_Termios_oflag () {
	return termios.c_oflag;
}

Flag Posix_TTY_Termios_cflag () {
	return termios.c_cflag;
}

Flag Posix_TTY_Termios_lflag () {
	return termios.c_lflag;
}

Cstring Posix_TTY_Termios_cc () {
	return (Cstring)termios.c_cc;
}

Speed Posix_TTY_Termios_cfgetospeed () {
	return cfgetospeed (&termios);
}

Speed Posix_TTY_Termios_cfgetispeed () {
	return cfgetispeed (&termios);
}

void Posix_TTY_Termios_setiflag (Flag f) {
	termios.c_iflag = f;
}

void Posix_TTY_Termios_setoflag (Flag f) {
	termios.c_oflag = f;
}

void Posix_TTY_Termios_setcflag (Flag f) {
	termios.c_cflag = f;
}

void Posix_TTY_Termios_setlflag (Flag f) {
	termios.c_lflag = f;
}

Int Posix_TTY_Termios_setospeed (Speed s) {
	return cfsetospeed (&termios, s);
}

Int Posix_TTY_Termios_setispeed (Speed s) {
	return cfsetispeed (&termios, s);
}

Int Posix_TTY_drain (Fd f) {
	return tcdrain (f);
}

Int Posix_TTY_flow (Fd f, Int i) {
	return tcflow (f, i);
}

Int Posix_TTY_flush (Fd f, Int i) {
	return tcflush (f, i);
}

Int Posix_TTY_getattr (Fd f) {
	return tcgetattr (f, &termios);
}

Int Posix_TTY_getpgrp (Fd f) {
	return tcgetpgrp (f);
}

Int Posix_TTY_sendbreak (Fd f, Int i) {
	return tcsendbreak (f, i);
}

Int Posix_TTY_setattr (Fd f, Int i) {
	return tcsetattr (f, i, &termios);
}

Int Posix_TTY_setpgrp (Fd f, Pid p) {
	return tcsetpgrp (f, p);
}
