#include "platform.h"

Int Posix_Process_execp (NullString f, Pointer a) {
	char		*file;
	char		*saved;
 	char		**args;
	int             n;
	int 		result;

	file = (char *) f;
	args = (typeof(args)) a;
	n = GC_arrayNumElements (a) - 1;
	saved = args[n];
	args[n] = (char *) NULL;
	result = execvp (file, (const char* const*) args);
	/* execp failed */
	args[n] = saved;
	return result;
}
