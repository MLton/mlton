#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "mlton-posix.h"

Int Posix_FileSys_open (NullString p, Word w, Mode m) {

#if (defined (__linux__) || defined (__FreeBSD__))

	return open ((char *) p, w, m);

#elif (defined (__CYGWIN__))

	struct stat buf;

	stat ((char *) p, &buf);
	if (S_ISDIR (buf.st_mode))
		return -1;
	return open((char *) p, w, m);

#else
	fprintf (stderr, "Posix_FileSys_open not defined for platform.\n");
#endif
}
