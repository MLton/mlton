#include "mlton-basis.h"

Char C_CS_sub(Cstring s, Int i) {
	return ((char*)(s))[i];
}

void C_CS_update(Cstring s, Int i, Char c) {
	((char*)(s))[i] = c;
}

Cstring C_CSS_sub(CstringArray a, Int i) {
	return (Cstring)(((char**)(a))[i]);
}
