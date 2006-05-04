#include "platform.h"

static struct termios termios;

C_TCFlag_t Posix_TTY_Termios_getIFlag (void) {
  return termios.c_iflag;
}

C_TCFlag_t Posix_TTY_Termios_getOFlag (void) {
  return termios.c_oflag;
}

C_TCFlag_t Posix_TTY_Termios_getCFlag (void) {
  return termios.c_cflag;
}

C_TCFlag_t Posix_TTY_Termios_getLFlag (void) {
  return termios.c_lflag;
}

void Posix_TTY_Termios_getCC (Array(C_CC_t) a) {
  for (int i = 0; i < NCCS; i++) 
    ((cc_t*)a)[i] = termios.c_cc[i];
}

C_Speed_t Posix_TTY_Termios_cfGetOSpeed (void) {
  return cfgetospeed (&termios);
}

C_Speed_t Posix_TTY_Termios_cfGetISpeed (void) {
  return cfgetispeed (&termios);
}

void Posix_TTY_Termios_setIFlag (C_TCFlag_t f) {
  termios.c_iflag = f;
}

void Posix_TTY_Termios_setOFlag (C_TCFlag_t f) {
  termios.c_oflag = f;
}

void Posix_TTY_Termios_setCFlag (C_TCFlag_t f) {
  termios.c_cflag = f;
}

void Posix_TTY_Termios_setLFlag (C_TCFlag_t f) {
  termios.c_lflag = f;
}

void Posix_TTY_Termios_setCC (Array(C_CC_t) a) {
  for (int i = 0; i < NCCS; i++) 
    termios.c_cc[i] = ((cc_t*)a)[i];
}

C_Errno_t(C_Int_t) Posix_TTY_Termios_cfSetOSpeed (C_Speed_t s) {
  return cfsetospeed (&termios, s);
}

C_Errno_t(C_Int_t) Posix_TTY_Termios_cfSetISpeed (C_Speed_t s) {
  return cfsetispeed (&termios, s);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_drain (C_Fd_t f) {
  return tcdrain (f);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_flow (C_Fd_t f, C_Int_t i) {
  return tcflow (f, i);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_flush (C_Fd_t f, C_Int_t i) {
  return tcflush (f, i);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_getattr (C_Fd_t f) {
  return tcgetattr (f, &termios);
}

C_Errno_t(C_PId_t) Posix_TTY_TC_getpgrp (C_Fd_t f) {
  return tcgetpgrp (f);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_sendbreak (C_Fd_t f, C_Int_t i) {
  return tcsendbreak (f, i);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_setattr (C_Fd_t f, C_Int_t i) {
  return tcsetattr (f, i, &termios);
}

C_Errno_t(C_Int_t) Posix_TTY_TC_setpgrp (C_Fd_t f, C_PId_t p) {
  return tcsetpgrp (f, p);
}
