#include <stdlib.h>
#include "mlton-posix.h"



#if (defined (__CYGWIN__) || defined (__FreeBSD__) || defined (__linux__))

Int Posix_ProcEnv_setenv (NullString s, NullString v) {
	return setenv ((char *)s, (char *)v, 1);
}

#elif (defined (__sun__))

#include <stdio.h>  // for sprintf
#include <strings.h>

/* This has a space leak, but I don't see how to avoid it, since the
 * specification of putenv is that it uses the memory for its arg.
 */

Int Posix_ProcEnv_setenv (NullString s, NullString v) {
	char *b;
	char *name;
	char *value;

	name = (char *)s;
	value = (char *)v;
	b = malloc (strlen (name) + strlen (value) + 2 /* = and \000 */);
	sprintf (b, "%s=%s", name, value);
	return putenv (b);
}

#else

#error Need to define Posix_ProcEnv_setenv for platform

#endif
