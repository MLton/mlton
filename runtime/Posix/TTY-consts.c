#include "platform.h"

const C_Int_t Posix_TTY_V_NCCS = NCCS;

const C_Int_t Posix_TTY_V_VEOF = VEOF;
const C_Int_t Posix_TTY_V_VEOL = VEOL;
const C_Int_t Posix_TTY_V_VERASE = VERASE;
const C_Int_t Posix_TTY_V_VINTR = VINTR;
const C_Int_t Posix_TTY_V_VKILL = VKILL;
const C_Int_t Posix_TTY_V_VMIN = VMIN;
const C_Int_t Posix_TTY_V_VQUIT = VQUIT;
const C_Int_t Posix_TTY_V_VSTART = VSTART;
const C_Int_t Posix_TTY_V_VSTOP = VSTOP;
const C_Int_t Posix_TTY_V_VSUSP = VSUSP;
const C_Int_t Posix_TTY_V_VTIME = VTIME;

const C_TCFlag_t Posix_TTY_I_BRKINT = BRKINT;
const C_TCFlag_t Posix_TTY_I_ICRNL = ICRNL;
const C_TCFlag_t Posix_TTY_I_IGNBRK = IGNBRK;
const C_TCFlag_t Posix_TTY_I_IGNCR = IGNCR;
const C_TCFlag_t Posix_TTY_I_IGNPAR = IGNPAR;
const C_TCFlag_t Posix_TTY_I_INLCR = INLCR;
const C_TCFlag_t Posix_TTY_I_INPCK = INPCK;
const C_TCFlag_t Posix_TTY_I_ISTRIP = ISTRIP;
const C_TCFlag_t Posix_TTY_I_IXANY = IXANY;
const C_TCFlag_t Posix_TTY_I_IXOFF = IXOFF;
const C_TCFlag_t Posix_TTY_I_IXON = IXON;
const C_TCFlag_t Posix_TTY_I_PARMRK = PARMRK;

const C_TCFlag_t Posix_TTY_O_OPOST = OPOST;
const C_TCFlag_t Posix_TTY_O_ONLCR = ONLCR;
const C_TCFlag_t Posix_TTY_O_OCRNL = OCRNL;
const C_TCFlag_t Posix_TTY_O_ONOCR = ONOCR;
const C_TCFlag_t Posix_TTY_O_ONLRET = ONLRET;
const C_TCFlag_t Posix_TTY_O_OFILL = OFILL;
const C_TCFlag_t Posix_TTY_O_NLDLY = NLDLY;
const C_TCFlag_t Posix_TTY_O_NL0 = NL0;
const C_TCFlag_t Posix_TTY_O_NL1 = NL1;
const C_TCFlag_t Posix_TTY_O_CRDLY = CRDLY;
const C_TCFlag_t Posix_TTY_O_CR0 = CR0;
const C_TCFlag_t Posix_TTY_O_CR1 = CR1;
const C_TCFlag_t Posix_TTY_O_CR2 = CR2;
const C_TCFlag_t Posix_TTY_O_CR3 = CR3;
const C_TCFlag_t Posix_TTY_O_TABDLY = TABDLY;
const C_TCFlag_t Posix_TTY_O_TAB0 = TAB0;
const C_TCFlag_t Posix_TTY_O_TAB1 = TAB1;
const C_TCFlag_t Posix_TTY_O_TAB2 = TAB2;
const C_TCFlag_t Posix_TTY_O_TAB3 = TAB3;
const C_TCFlag_t Posix_TTY_O_BSDLY = BSDLY;
const C_TCFlag_t Posix_TTY_O_BS0 = BS0;
const C_TCFlag_t Posix_TTY_O_BS1 = BS1;
const C_TCFlag_t Posix_TTY_O_VTDLY = VTDLY;
const C_TCFlag_t Posix_TTY_O_VT0 = VT0;
const C_TCFlag_t Posix_TTY_O_VT1 = VT1;
const C_TCFlag_t Posix_TTY_O_FFDLY = FFDLY;
const C_TCFlag_t Posix_TTY_O_FF0 = FF0;
const C_TCFlag_t Posix_TTY_O_FF1 = FF1;

const C_TCFlag_t Posix_TTY_C_CSIZE = CSIZE;
const C_TCFlag_t Posix_TTY_C_CS5 = CS5;
const C_TCFlag_t Posix_TTY_C_CS6 = CS6;
const C_TCFlag_t Posix_TTY_C_CS7 = CS7;
const C_TCFlag_t Posix_TTY_C_CS8 = CS8;
const C_TCFlag_t Posix_TTY_C_CSTOPB = CSTOPB;
const C_TCFlag_t Posix_TTY_C_CREAD = CREAD;
const C_TCFlag_t Posix_TTY_C_PARENB = PARENB;
const C_TCFlag_t Posix_TTY_C_PARODD = PARODD;
const C_TCFlag_t Posix_TTY_C_HUPCL = HUPCL;
const C_TCFlag_t Posix_TTY_C_CLOCAL = CLOCAL;

const C_TCFlag_t Posix_TTY_L_ECHO = ECHO;
const C_TCFlag_t Posix_TTY_L_ECHOE = ECHOE;
const C_TCFlag_t Posix_TTY_L_ECHOK = ECHOK;
const C_TCFlag_t Posix_TTY_L_ECHONL = ECHONL;
const C_TCFlag_t Posix_TTY_L_ICANON = ICANON;
const C_TCFlag_t Posix_TTY_L_IEXTEN = IEXTEN;
const C_TCFlag_t Posix_TTY_L_ISIG = ISIG;
const C_TCFlag_t Posix_TTY_L_NOFLSH = NOFLSH;
const C_TCFlag_t Posix_TTY_L_TOSTOP = TOSTOP;

const C_Speed_t Posix_TTY_B0 = B0;
const C_Speed_t Posix_TTY_B50 = B50;
const C_Speed_t Posix_TTY_B75 = B75;
const C_Speed_t Posix_TTY_B110 = B110;
const C_Speed_t Posix_TTY_B134 = B134;
const C_Speed_t Posix_TTY_B150 = B150;
const C_Speed_t Posix_TTY_B200 = B200;
const C_Speed_t Posix_TTY_B300 = B300;
const C_Speed_t Posix_TTY_B600 = B600;
const C_Speed_t Posix_TTY_B1200 = B1200;
const C_Speed_t Posix_TTY_B1800 = B1800;
const C_Speed_t Posix_TTY_B2400 = B2400;
const C_Speed_t Posix_TTY_B4800 = B4800;
const C_Speed_t Posix_TTY_B9600 = B9600;
const C_Speed_t Posix_TTY_B19200 = B19200;
const C_Speed_t Posix_TTY_B38400 = B38400;

const C_Int_t Posix_TTY_TC_TCSADRAIN = TCSADRAIN;
const C_Int_t Posix_TTY_TC_TCSAFLUSH = TCSAFLUSH;
const C_Int_t Posix_TTY_TC_TCSANOW = TCSANOW;

const C_Int_t Posix_TTY_TC_TCIOFF = TCIOFF;
const C_Int_t Posix_TTY_TC_TCION = TCION;
const C_Int_t Posix_TTY_TC_TCOOFF = TCOOFF;
const C_Int_t Posix_TTY_TC_TCOON = TCOON;

const C_Int_t Posix_TTY_TC_TCIFLUSH = TCIFLUSH;
const C_Int_t Posix_TTY_TC_TCIOFLUSH = TCIOFLUSH;
const C_Int_t Posix_TTY_TC_TCOFLUSH = TCOFLUSH;
