#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

Int Posix_FileSys_open (NullString p, Word w, Mode m) {
	Int res;

	res = open ((char *) p, w, m);

	if (DEBUG)
		fprintf (stderr, "%d = Posix_FileSys_open (%s, 0x%08x, 0x%08x)\n", 
				(int)res, 
				(char *)p, 
				(unsigned int)w,
				(unsigned int)m);
	return res;
}
