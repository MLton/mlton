#include <string.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

extern struct GC_state gcState;

#define	MAGIC	"MLton prof\n"

extern void	_start(void),
		etext(void);

#define START ((uint)&_start)
#define END (uint)&etext

Pointer MLton_ProfileAlloc_current (void) {
	return (Pointer)gcState.profileAllocCounts;
}

void MLton_ProfileAlloc_setCurrent (Pointer d) {
	gcState.profileAllocCounts = (ullong*)d;
}

void MLton_ProfileAlloc_inc (Word amount) {
	assert (gcState.profileAllocIsOn);
	if (FALSE)
		fprintf (stderr, "MLton_ProfileAlloc_inc (%u, %u)\n",
				gcState.profileAllocIndex,
				(uint)amount);
	gcState.profileAllocCounts[gcState.profileAllocIndex] += amount;
}

Pointer MLton_ProfileAlloc_Data_malloc (void) {
/* Note, perhaps this code should use mmap()/munmap() instead of
 * malloc()/free() for the array of bins.
 */
	ullong *data;

	assert (gcState.profileAllocIsOn);
	data = (ullong*) malloc (gcState.profileAllocNumLabels * sizeof (*data));
	if (data == NULL)
		die ("Out of memory");
	MLton_ProfileAlloc_Data_reset ((Pointer)data);
	return (Pointer)data;
}

void MLton_ProfileAlloc_Data_free (Pointer d) {
	ullong *data;

	assert (gcState.profileAllocIsOn);
	data = (ullong*)d;
	assert (data != NULL);
	free (data);
}

void MLton_ProfileAlloc_Data_reset (Pointer d) {
	uint *data;

	assert (gcState.profileAllocIsOn);
	data = (uint*)d;
	assert (data != NULL);
	memset (data, 0, gcState.profileAllocNumLabels * sizeof(*data));
}

void MLton_ProfileAlloc_Data_write (Pointer d, Word fd) {
/* Write a profile data array out to a file descriptor
 * The file consists of:
 *	a 12 byte magic value ("MLton prof\n\000")
 *	the lowest address corresponding to a bin
 *	just past the highest address corresponding to a bin
 *	the counter size in bytes (4 or 8)
 *	the bins
 */
	ullong *data;
	uint i;

	assert (gcState.profileAllocIsOn);
	data = (ullong*)d;
	swrite (fd, MAGIC, sizeof(MAGIC));
	swriteUint (fd, gcState.magic);
	swriteUint (fd, START);
	swriteUint (fd, END);
	swriteUint (fd, sizeof(*data));
	swriteUint (fd, MLPROF_KIND_ALLOC);
	for (i = 0; i < gcState.profileAllocNumLabels; ++i) {
		if (data[i] > 0) {
			swriteUint (fd, gcState.profileAllocLabels[i]);
			swriteUllong (fd, data[i]);
		}
	}
}
