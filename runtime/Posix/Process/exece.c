#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 200112L

#include "platform.h"

Int Posix_Process_exece (NullString p, Pointer a, Pointer e) {
	char		*path;
	char		*asaved;
	char 		*esaved;
	char 		**args;
	char 		**env;
	int             an;
	int 		en;
	int 		result;

	path = (char *) p;
	args = (char **) a;
	env = (char **) e;
	an = GC_arrayNumElements (a) - 1;
	asaved = args[an];
	en = GC_arrayNumElements (e) - 1;
	esaved = env[en];
	args[an] = (char *) NULL;
	env[en] = (char *) NULL;
	result = EXECVE (path, args, env);
	/* exece failed */
	args[an] = asaved;
	env[en] = esaved;
	return result;
}
