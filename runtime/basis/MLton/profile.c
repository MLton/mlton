#include <string.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

enum {
	DEBUG_PROFILE = FALSE,
};

extern struct GC_state gcState;

void MLton_Profile_Data_write (Pointer p, Word fd) {
	GC_profileWrite (&gcState, (GC_profile)p, (int)fd);
}

void MLton_Profile_Data_free (Pointer p) {
	GC_profileFree (&gcState, (GC_profile)p);
}

Pointer MLton_Profile_Data_malloc (void) {
	return (Pointer)GC_profileNew (&gcState);
}

Pointer MLton_Profile_current (void) {
	GC_state s;
	Pointer res;

	s = &gcState;
	res = (Pointer)s->profile;
	if (DEBUG_PROFILE)
		fprintf (stderr, "0x%0x8 = MLton_Profile_current ()\n", 
				(uint)res);
	return res;
}

void MLton_Profile_done () {
	GC_profileDone (&gcState);
}

void MLton_Profile_enter (Word sourceSeqsIndex) {
	GC_profileEnter (&gcState, sourceSeqsIndex);
}

/* gcState.currentSource should be set before calling MLton_Profile_inc. */
void MLton_Profile_inc (Word amount) {
	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_inc\n");
	GC_profileInc (&gcState, amount);
}

void MLton_Profile_leave (Word sourceSeqsIndex) {
	GC_profileLeave (&gcState, sourceSeqsIndex);
}

void MLton_Profile_setCurrent (Pointer d) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE)
		fprintf (stderr, "MLton_Profile_setCurrent (0x%08x)\n", (uint)d);
	s->profile = (GC_profile)d;
}
