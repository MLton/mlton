#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "mlton-posix.h"

enum {
	DEBUG = 0,
};

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
