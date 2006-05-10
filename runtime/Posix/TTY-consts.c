#include "platform.h"

const C_Int_t Posix_TTY_V_NCCS = NCCS;

#ifndef VEOF
#define VEOF -1
#endif
const C_Int_t Posix_TTY_V_VEOF = VEOF;
#ifndef VEOL
#define VEOL -1
#endif
const C_Int_t Posix_TTY_V_VEOL = VEOL;
#ifndef VERASE
#define VERASE -1
#endif
const C_Int_t Posix_TTY_V_VERASE = VERASE;
#ifndef VINTR
#define VINTR -1
#endif
const C_Int_t Posix_TTY_V_VINTR = VINTR;
#ifndef VKILL
#define VKILL -1
#endif
const C_Int_t Posix_TTY_V_VKILL = VKILL;
#ifndef VMIN
#define VMIN -1
#endif
const C_Int_t Posix_TTY_V_VMIN = VMIN;
#ifndef VQUIT
#define VQUIT -1
#endif
const C_Int_t Posix_TTY_V_VQUIT = VQUIT;
#ifndef VSTART
#define VSTART -1
#endif
const C_Int_t Posix_TTY_V_VSTART = VSTART;
#ifndef VSTOP
#define VSTOP -1
#endif
const C_Int_t Posix_TTY_V_VSTOP = VSTOP;
#ifndef VSUSP
#define VSUSP -1
#endif
const C_Int_t Posix_TTY_V_VSUSP = VSUSP;
#ifndef VTIME
#define VTIME -1
#endif
const C_Int_t Posix_TTY_V_VTIME = VTIME;

#ifndef BRKINT
#define BRKINT -1
#endif
const C_TCFlag_t Posix_TTY_I_BRKINT = BRKINT;
#ifndef ICRNL
#define ICRNL -1
#endif
const C_TCFlag_t Posix_TTY_I_ICRNL = ICRNL;
#ifndef IGNBRK
#define IGNBRK -1
#endif
const C_TCFlag_t Posix_TTY_I_IGNBRK = IGNBRK;
#ifndef IGNCR
#define IGNCR -1
#endif
const C_TCFlag_t Posix_TTY_I_IGNCR = IGNCR;
#ifndef IGNPAR
#define IGNPAR -1
#endif
const C_TCFlag_t Posix_TTY_I_IGNPAR = IGNPAR;
#ifndef INLCR
#define INLCR -1
#endif
const C_TCFlag_t Posix_TTY_I_INLCR = INLCR;
#ifndef INPCK
#define INPCK -1
#endif
const C_TCFlag_t Posix_TTY_I_INPCK = INPCK;
#ifndef ISTRIP
#define ISTRIP -1
#endif
const C_TCFlag_t Posix_TTY_I_ISTRIP = ISTRIP;
#ifndef IXANY
#define IXANY -1
#endif
const C_TCFlag_t Posix_TTY_I_IXANY = IXANY;
#ifndef IXOFF
#define IXOFF -1
#endif
const C_TCFlag_t Posix_TTY_I_IXOFF = IXOFF;
#ifndef IXON
#define IXON -1
#endif
const C_TCFlag_t Posix_TTY_I_IXON = IXON;
#ifndef PARMRK
#define PARMRK -1
#endif
const C_TCFlag_t Posix_TTY_I_PARMRK = PARMRK;

#ifndef OPOST
#define OPOST -1
#endif
const C_TCFlag_t Posix_TTY_O_OPOST = OPOST;
#ifndef ONLCR
#define ONLCR -1
#endif
const C_TCFlag_t Posix_TTY_O_ONLCR = ONLCR;
#ifndef OCRNL
#define OCRNL -1
#endif
const C_TCFlag_t Posix_TTY_O_OCRNL = OCRNL;
#ifndef ONOCR
#define ONOCR -1
#endif
const C_TCFlag_t Posix_TTY_O_ONOCR = ONOCR;
#ifndef ONLRET
#define ONLRET -1
#endif
const C_TCFlag_t Posix_TTY_O_ONLRET = ONLRET;
#ifndef OFILL
#define OFILL -1
#endif
const C_TCFlag_t Posix_TTY_O_OFILL = OFILL;
#ifndef NLDLY
#define NLDLY -1
#endif
const C_TCFlag_t Posix_TTY_O_NLDLY = NLDLY;
#ifndef NL0
#define NL0 -1
#endif
const C_TCFlag_t Posix_TTY_O_NL0 = NL0;
#ifndef NL1
#define NL1 -1
#endif
const C_TCFlag_t Posix_TTY_O_NL1 = NL1;
#ifndef CRDLY
#define CRDLY -1
#endif
const C_TCFlag_t Posix_TTY_O_CRDLY = CRDLY;
#ifndef CR0
#define CR0 -1
#endif
const C_TCFlag_t Posix_TTY_O_CR0 = CR0;
#ifndef CR1
#define CR1 -1
#endif
const C_TCFlag_t Posix_TTY_O_CR1 = CR1;
#ifndef CR2
#define CR2 -1
#endif
const C_TCFlag_t Posix_TTY_O_CR2 = CR2;
#ifndef CR3
#define CR3 -1
#endif
const C_TCFlag_t Posix_TTY_O_CR3 = CR3;
#ifndef TABDLY
#define TABDLY -1
#endif
const C_TCFlag_t Posix_TTY_O_TABDLY = TABDLY;
#ifndef TAB0
#define TAB0 -1
#endif
const C_TCFlag_t Posix_TTY_O_TAB0 = TAB0;
#ifndef TAB1
#define TAB1 -1
#endif
const C_TCFlag_t Posix_TTY_O_TAB1 = TAB1;
#ifndef TAB2
#define TAB2 -1
#endif
const C_TCFlag_t Posix_TTY_O_TAB2 = TAB2;
#ifndef TAB3
#define TAB3 -1
#endif
const C_TCFlag_t Posix_TTY_O_TAB3 = TAB3;
#ifndef BSDLY
#define BSDLY -1
#endif
const C_TCFlag_t Posix_TTY_O_BSDLY = BSDLY;
#ifndef BS0
#define BS0 -1
#endif
const C_TCFlag_t Posix_TTY_O_BS0 = BS0;
#ifndef BS1
#define BS1 -1
#endif
const C_TCFlag_t Posix_TTY_O_BS1 = BS1;
#ifndef VTDLY
#define VTDLY -1
#endif
const C_TCFlag_t Posix_TTY_O_VTDLY = VTDLY;
#ifndef VT0
#define VT0 -1
#endif
const C_TCFlag_t Posix_TTY_O_VT0 = VT0;
#ifndef VT1
#define VT1 -1
#endif
const C_TCFlag_t Posix_TTY_O_VT1 = VT1;
#ifndef FFDLY
#define FFDLY -1
#endif
const C_TCFlag_t Posix_TTY_O_FFDLY = FFDLY;
#ifndef FF0
#define FF0 -1
#endif
const C_TCFlag_t Posix_TTY_O_FF0 = FF0;
#ifndef FF1
#define FF1 -1
#endif
const C_TCFlag_t Posix_TTY_O_FF1 = FF1;

#ifndef CSIZE
#define CSIZE -1
#endif
const C_TCFlag_t Posix_TTY_C_CSIZE = CSIZE;
#ifndef CS5
#define CS5 -1
#endif
const C_TCFlag_t Posix_TTY_C_CS5 = CS5;
#ifndef CS6
#define CS6 -1
#endif
const C_TCFlag_t Posix_TTY_C_CS6 = CS6;
#ifndef CS7
#define CS7 -1
#endif
const C_TCFlag_t Posix_TTY_C_CS7 = CS7;
#ifndef CS8
#define CS8 -1
#endif
const C_TCFlag_t Posix_TTY_C_CS8 = CS8;
#ifndef CSTOPB
#define CSTOPB -1
#endif
const C_TCFlag_t Posix_TTY_C_CSTOPB = CSTOPB;
#ifndef CREAD
#define CREAD -1
#endif
const C_TCFlag_t Posix_TTY_C_CREAD = CREAD;
#ifndef PARENB
#define PARENB -1
#endif
const C_TCFlag_t Posix_TTY_C_PARENB = PARENB;
#ifndef PARODD
#define PARODD -1
#endif
const C_TCFlag_t Posix_TTY_C_PARODD = PARODD;
#ifndef HUPCL
#define HUPCL -1
#endif
const C_TCFlag_t Posix_TTY_C_HUPCL = HUPCL;
#ifndef CLOCAL
#define CLOCAL -1
#endif
const C_TCFlag_t Posix_TTY_C_CLOCAL = CLOCAL;

#ifndef ECHO
#define ECHO -1
#endif
const C_TCFlag_t Posix_TTY_L_ECHO = ECHO;
#ifndef ECHOE
#define ECHOE -1
#endif
const C_TCFlag_t Posix_TTY_L_ECHOE = ECHOE;
#ifndef ECHOK
#define ECHOK -1
#endif
const C_TCFlag_t Posix_TTY_L_ECHOK = ECHOK;
#ifndef ECHONL
#define ECHONL -1
#endif
const C_TCFlag_t Posix_TTY_L_ECHONL = ECHONL;
#ifndef ICANON
#define ICANON -1
#endif
const C_TCFlag_t Posix_TTY_L_ICANON = ICANON;
#ifndef IEXTEN
#define IEXTEN -1
#endif
const C_TCFlag_t Posix_TTY_L_IEXTEN = IEXTEN;
#ifndef ISIG
#define ISIG -1
#endif
const C_TCFlag_t Posix_TTY_L_ISIG = ISIG;
#ifndef NOFLSH
#define NOFLSH -1
#endif
const C_TCFlag_t Posix_TTY_L_NOFLSH = NOFLSH;
#ifndef TOSTOP
#define TOSTOP -1
#endif
const C_TCFlag_t Posix_TTY_L_TOSTOP = TOSTOP;

#ifndef B0
#define B0 -1
#endif
const C_Speed_t Posix_TTY_B0 = B0;
#ifndef B50
#define B50 -1
#endif
const C_Speed_t Posix_TTY_B50 = B50;
#ifndef B75
#define B75 -1
#endif
const C_Speed_t Posix_TTY_B75 = B75;
#ifndef B110
#define B110 -1
#endif
const C_Speed_t Posix_TTY_B110 = B110;
#ifndef B134
#define B134 -1
#endif
const C_Speed_t Posix_TTY_B134 = B134;
#ifndef B150
#define B150 -1
#endif
const C_Speed_t Posix_TTY_B150 = B150;
#ifndef B200
#define B200 -1
#endif
const C_Speed_t Posix_TTY_B200 = B200;
#ifndef B300
#define B300 -1
#endif
const C_Speed_t Posix_TTY_B300 = B300;
#ifndef B600
#define B600 -1
#endif
const C_Speed_t Posix_TTY_B600 = B600;
#ifndef B1200
#define B1200 -1
#endif
const C_Speed_t Posix_TTY_B1200 = B1200;
#ifndef B1800
#define B1800 -1
#endif
const C_Speed_t Posix_TTY_B1800 = B1800;
#ifndef B2400
#define B2400 -1
#endif
const C_Speed_t Posix_TTY_B2400 = B2400;
#ifndef B4800
#define B4800 -1
#endif
const C_Speed_t Posix_TTY_B4800 = B4800;
#ifndef B9600
#define B9600 -1
#endif
const C_Speed_t Posix_TTY_B9600 = B9600;
#ifndef B19200
#define B19200 -1
#endif
const C_Speed_t Posix_TTY_B19200 = B19200;
#ifndef B38400
#define B38400 -1
#endif
const C_Speed_t Posix_TTY_B38400 = B38400;

#ifndef TCSADRAIN
#define TCSADRAIN -1
#endif
const C_Int_t Posix_TTY_TC_TCSADRAIN = TCSADRAIN;
#ifndef TCSAFLUSH
#define TCSAFLUSH -1
#endif
const C_Int_t Posix_TTY_TC_TCSAFLUSH = TCSAFLUSH;
#ifndef TCSANOW
#define TCSANOW -1
#endif
const C_Int_t Posix_TTY_TC_TCSANOW = TCSANOW;

#ifndef TCIOFF
#define TCIOFF -1
#endif
const C_Int_t Posix_TTY_TC_TCIOFF = TCIOFF;
#ifndef TCION
#define TCION -1
#endif
const C_Int_t Posix_TTY_TC_TCION = TCION;
#ifndef TCOOFF
#define TCOOFF -1
#endif
const C_Int_t Posix_TTY_TC_TCOOFF = TCOOFF;
#ifndef TCOON
#define TCOON -1
#endif
const C_Int_t Posix_TTY_TC_TCOON = TCOON;

#ifndef TCIFLUSH
#define TCIFLUSH -1
#endif
const C_Int_t Posix_TTY_TC_TCIFLUSH = TCIFLUSH;
#ifndef TCIOFLUSH
#define TCIOFLUSH -1
#endif
const C_Int_t Posix_TTY_TC_TCIOFLUSH = TCIOFLUSH;
#ifndef TCOFLUSH
#define TCOFLUSH -1
#endif
const C_Int_t Posix_TTY_TC_TCOFLUSH = TCOFLUSH;
