#if (defined (__linux__) || defined (__FreeBSD__))
#include <string.h>

#include "gc.h"
#include "mlton-basis.h"
#include "my-lib.h"

enum {
	DEBUG_PROFILE_ALLOC = FALSE,
};

extern struct GC_state gcState;

Pointer MLton_ProfileAlloc_current (void) {
	Pointer res;

	res = (Pointer)gcState.profileAllocCounts;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "0x%0x8 = MLton_ProfileAlloc_current ()\n",
				(uint)res);
	return res;
}

void MLton_ProfileAlloc_setCurrent (Pointer d) {
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_setCurrent (0x%08x)\n",
				(uint)d);
	gcState.profileAllocCounts = (ullong*)d;
}

void MLton_ProfileAlloc_inc (Word amount) {
	GC_state s;

	s = &gcState;
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_inc (%u, %u)\n",
				s->profileAllocIndex,
				(uint)amount);
	assert (s->profileAllocIsOn);
	assert (s->profileAllocIndex < s->profileSourceSeqsSize);
	s->profileAllocCounts [s->profileSourceSeqs [s->profileAllocIndex] [1]] 
		+= amount;
}

Pointer MLton_ProfileAlloc_Data_malloc (void) {
/* Note, perhaps this code should use mmap()/munmap() instead of
 * malloc()/free() for the array of bins.
 */
	ullong *data;

	assert (gcState.profileAllocIsOn);
	data = (ullong*) malloc (gcState.profileSourcesSize * sizeof (*data));
	if (data == NULL)
		die ("Out of memory");
	MLton_ProfileAlloc_Data_reset ((Pointer)data);
	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "0x%08x = MLton_ProfileAlloc_Data_malloc ()\n",
				(uint)data);
	return (Pointer)data;
}

void MLton_ProfileAlloc_Data_free (Pointer d) {
	ullong *data;

	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_Data_free (0x%08x)\n",
				(uint)d);
	assert (gcState.profileAllocIsOn);
	data = (ullong*)d;
	assert (data != NULL);
	free (data);
}

void MLton_ProfileAlloc_Data_reset (Pointer d) {
	uint *data;

	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_Data_reset (0x%08x)\n",
				(uint)data);
	assert (gcState.profileAllocIsOn);
	data = (uint*)d;
	assert (data != NULL);
	memset (data, 0, gcState.profileSourcesSize * sizeof(*data));
}

static void writeString (int fd, string s) {
	swrite (fd, s, strlen(s));
	swrite (fd, "\n", 1);
}

static void writeWord (int fd, word w) {
	char buf[20];

	sprintf (buf, "0x%08x", w);
	writeString (fd, buf);
}

static void writeUllong (int fd, ullong u) {
	char buf[20];

	sprintf (buf, "%llu", u);
	writeString (fd, buf);
}

void MLton_ProfileAlloc_Data_write (Pointer d, Word fd) {
/* Write a profile data array out to a file descriptor */
	ullong *data;
	uint i;

	if (DEBUG_PROFILE_ALLOC)
		fprintf (stderr, "MLton_ProfileAlloc_Data_write (0x%08x, %d)\n",
				(uint)d, fd);
	assert (gcState.profileAllocIsOn);
	data = (ullong*)d;
	writeString (fd, "MLton prof");
	writeString (fd, "alloc");
	writeWord (fd, gcState.magic);
	for (i = 0; i < gcState.profileSourcesSize; ++i)
		writeUllong (fd, data[i]);
}

#elif (defined (__CYGWIN__))

/* No profiling on Cygwin. 
 * There is a check in mlton/main/main.sml to make sure that profiling is never
 * turned on on Cygwin.
 */

/* We have to put some stubs here because the runtime initialization code uses
 * them.
 */
#include "mlton-basis.h"

Pointer MLton_ProfileAlloc_Data_malloc (void) {
	die ("no allocation profiling on Cygwin");
}

void MLton_ProfileAlloc_setCurrent (Pointer d) {
	die ("no allocation profiling on Cygwin");
}

#else

#error profiling not defined

#endif
