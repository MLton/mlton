#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "mlton-posix.h"

/* FreeBSD uses 64 bits files by default, so doesn't have O_LARGEFILE. */
#if (defined __FreeBSD__)
#define O_LARGEFILE 0
#endif

Int Posix_FileSys_open (NullString p, Word w, Mode m) {

#if (defined (__linux__) || defined (__FreeBSD__) || defined (__sun__))

	return open ((char *) p, w | O_LARGEFILE, m);

#elif (defined (__CYGWIN__))

	struct stat buf;

	stat ((char *) p, &buf);
	if (S_ISDIR (buf.st_mode))
		return -1;
	return open ((char *) p, w, m);

#else

#error Posix_FileSys_open not defined

#endif
}
