#include "mlton-basis.h"
#include "prof.h"

void MLton_Profile_reset (void)
{
	resetProf();
}

void MLton_Profile_write (Cstring name) 
{
	writeProf((char*)name);
}
