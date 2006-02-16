#include "platform.h"

void MLton_bug (Pointer msg) {
        fprintf (stderr, "MLton bug: %s.\n%s\n",
                        (char*)msg,
                        "Please send a bug report to MLton@mlton.org.");
        exit (2);
}
