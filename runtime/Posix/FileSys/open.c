#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "mlton-posix.h"

enum {
	DEBUG = 0,
};

/* {Free,Net}BSD use 64 bits files by default, so doesn't have O_LARGEFILE. */
#if (defined (__FreeBSD__) || defined (__NetBSD__))
#define O_LARGEFILE 0
#endif

Int Posix_FileSys_open (NullString p, Word w, Mode m) {
	Int res;

#if (defined (__FreeBSD__) || defined (__linux__) || defined (__NetBSD__) || defined (__sun__))

	res = open ((char *) p, w | O_LARGEFILE, m);

#elif (defined (__CYGWIN__))

	struct stat buf;

	stat ((char *) p, &buf);
	if (S_ISDIR (buf.st_mode))
		res = -1;
	else
		res = open ((char *) p, w, m);

#else

#error Posix_FileSys_open not defined

#endif

	if (DEBUG)
		fprintf (stderr, "%d = Posix_FileSys_open (%s, 0x%08x, 0x%08x)\n", 
				(int)res, 
				(char *)p, 
				(unsigned int)w,
				(unsigned int)m);
	return res;
}
