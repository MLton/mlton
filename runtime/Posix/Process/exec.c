#include <unistd.h>
#include "gc.h"
#include "mlton-posix.h"

Int Posix_Process_exec(NullString p, Pointer a) {
	char		*path;
	char 		*saved;
	char 		**args;
	int             n;
	int 		result;

	path = (char *) p;
	args = (char **) a;
	n = GC_arrayNumElements(a) - 1;
	saved = args[n];
	args[n] = (char *) NULL;
	result = execv(path, args);
	/* exec failed */
	args[n] = saved;
	return result;
}
