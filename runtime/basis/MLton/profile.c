#include "platform.h"

#ifndef DEBUG_PROFILE
#define	DEBUG_PROFILE FALSE
#endif

extern struct GC_state gcState;

void MLton_Profile_Data_free (Pointer p) {
	GC_profileFree (&gcState, (GC_profile)p);
}

Pointer MLton_Profile_Data_malloc (void) {
	return (Pointer)GC_profileNew (&gcState);
}

void MLton_Profile_Data_write (Pointer p, Word fd) {
	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_Data_write (0x%08x)\n", (uint)p);
	GC_profileWrite (&gcState, (GC_profile)p, (int)fd);
}

Pointer MLton_Profile_current (void) {
	GC_state s;
	Pointer res;

	s = &gcState;
	res = (Pointer)s->profile;
	if (DEBUG_PROFILE)
		fprintf (stderr, "0x%08x = MLton_Profile_current ()\n", 
				(uint)res);
	return res;
}

void MLton_Profile_done () {
	GC_profileDone (&gcState);
}

void MLton_Profile_setCurrent (Pointer d) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_setCurrent (0x%08x)\n", (uint)d);
	s->profile = (GC_profile)d;
}
