#include "mlton-basis.h"
#include "my-lib.h"
#include "stdarg.h"
#include "string.h"

static Bool argB[10];
static Char argC[10];
static Int argI[10];
static Real argR[10];
static Word argW[10];

static Bool resB;
static Char resC;
static Int resI;
static Real resR;
static Word resW;

Cstring callbackName;
Cstring callbackType;

Cstring MLton_Callback_callbackName() {
  return callbackName;
}

Cstring MLton_Callback_callbackType() {
  return callbackType;
}

/* SML functions */
Bool MLton_Callback_fetchB(Int l) {
  return argB[l];
}

Char MLton_Callback_fetchC(Int l) {
  return argC[l];
}

Int MLton_Callback_fetchI(Int l) {
  return argI[l];
}

Real MLton_Callback_fetchR(Int l) {
  return argR[l];
}

Word MLton_Callback_fetchW(Int l) {
  return argW[l];
}

void MLton_Callback_retB(Bool b) {
  resB = b;
}

void MLton_Callback_retC(Char c) {
  resC = c;
}

void MLton_Callback_retI(Int i) {
  resI = i;
}

void MLton_Callback_retR(Real r) {
  resR = r;
}

void MLton_Callback_retW(Word w) {
  resW = w;
}

/* C function */
void MLton_callFromC ();
int MLton_Callback_call(char *rep, char *name, ...) {
  int len, i;
  int indices[5] = {0,0,0,0,0};
  va_list ap;

  len = strlen(rep);
  for (i = 0; i < len; i++) {
    switch(rep[i]) {
    case 'B':
    case 'C':
    case 'I':
    case 'R':
    case 'U':
    case 'W':
      break;
    default: 
      return -1;
    }
  }

  va_start(ap, name);
  for (i = 0; i < len - 1; i++) {
    switch (rep[i]) {
    case 'B':
      argB[indices[0]++] = va_arg(ap, Bool);
      break;
    case 'C':
      argC[indices[1]++] = (Char)va_arg(ap, int);
      break;
    case 'I':
      argI[indices[2]++] = va_arg(ap, Int);
      break;
    case 'R':
      argR[indices[3]++] = va_arg(ap, Real);
      break;
    case 'U':
      break;
    case 'W':
      argW[indices[4]++] = va_arg(ap, Word);
      break;
    }
  }
  callbackName = (uint)name;
  callbackType = (uint)rep;
  MLton_callFromC();
  switch (rep[len-1]) {
  case 'B':
    *(va_arg(ap, Bool*)) = resB;
    break;
  case 'C':
    *(va_arg(ap, Char*)) = resC;
    break;
  case 'I':
    *(va_arg(ap, Int*)) = resI;
    break;
  case 'R':
    *(va_arg(ap, Real*)) = resR;
    break;
  case 'U':
    break;
  case 'W':
    *(va_arg(ap, Word*)) = resW;
    break;
  }
  va_end(ap);

  return 1;
}
