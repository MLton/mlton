#include <signal.h>
#include "gc.h"
#include "mlton-posix.h"

enum {
	DEBUG_SIGNALS = FALSE,
};

extern struct GC_state gcState;

bool Posix_Signal_resetPending () {
	if (DEBUG_SIGNALS)
		fprintf (stderr, "Posix_Signal_resetPending ()\n");
	sigemptyset (&gcState.signalsPending);
	gcState.gcSignalIsPending = FALSE;
}
