/* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
#include <math.h>
#include <string.h>
#include "libmlton.h"

void MLton_printStringEscaped (FILE *f, unsigned char *s) {
	int i;
	for (i = 0; s[i] != '\0'; i++)
		fprintf(f, "%d%d%d", 
				s[i] / 100 % 10,
				s[i] / 10 % 10,
				s[i] % 10);
	fprintf(f, "\n");
}

/* ------------------------------------------------- */
/*                     MLton_init                     */
/* ------------------------------------------------- */

extern char **environ; /* for Posix_ProcEnv_environ */

void MLton_init (int argc, char **argv, GC_state s) {
	int start;

	Posix_ProcEnv_environ = (CstringArray)environ;
	start = GC_init (s, argc, argv);
	/* Setup argv and argc that SML sees. */
	/* start is now the index of the first real arg. */
	CommandLine_commandName = (uint)(argv[0]);
	CommandLine_argc = argc - start;
	CommandLine_argv = (uint)(argv + start);
}
