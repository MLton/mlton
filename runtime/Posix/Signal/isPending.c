#include <signal.h>
#include "gc.h"
#include "mlton-posix.h"

enum {
	DEBUG_SIGNALS = FALSE,
};

extern struct GC_state gcState;

bool Posix_Signal_isGCPending () {
	Bool res;

	res = gcState.gcSignalIsPending;
	if (DEBUG_SIGNALS)
		fprintf (stderr, "%s = Posix_Signal_isGCPending ()\n",
				boolToString (res));
 	return res;
}

bool Posix_Signal_isPending (Int signum) {
	return sigismember (&gcState.signalsPending, signum);
}
