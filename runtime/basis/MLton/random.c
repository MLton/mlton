#include "mlton-basis.h"
#include "my-lib.h"

/* Linux specific.  Uses /dev/random to get a random word. */
Word MLton_random() {
	FILE *file;
	Word result;

	file = sfopen("/dev/random", "r");
	result = sfreadUint(file);
	fclose(file);
	return result;
}
