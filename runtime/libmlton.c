/* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 */
#include <math.h>
#include <string.h>
#include "libmlton.h"

void MLton_printStringEscaped(FILE *f, unsigned char *s) {
	int i;
	for (i = 0; s[i] != '\0'; i++)
		fprintf(f, "%d%d%d", 
				s[i] / 100 % 10,
				s[i] / 10 % 10,
				s[i] % 10);
	fprintf(f, "\n");
}

static void usage(string s) {
	die("Usage: %s [@MLton [fixed-heap n[{k|m}]] [gc-messages] [gc-summary] [load-world file] [max-heap n[{k|m}]] [ram-slop x] --] args", 
		s);
}

static float stringToFloat(string s) {
	float f;

	sscanf(s, "%f", &f);
	return f;
}

static uint stringToBytes(string s) {
	char c;
	uint result;
	int i, m;
	
	result = 0;
	i = 0;

	while ((c = s[i++]) != '\000') {
		switch (c) {
		case 'm':
			if (s[i] == '\000') 
				result = result * 1048576;
			else return 0;
			break;
		case 'k':
			if (s[i] == '\000') 
				result = result * 1024;
			else return 0;
			break;
		default:
			m = (int)(c - '0');
			if (0 <= m and m <= 9)
				result = result * 10 + m;
			else return 0;
		}
	}
	
	return result;
}

/* ------------------------------------------------- */
/*                     MLton_init                     */
/* ------------------------------------------------- */

extern char **environ; /* for Posix_ProcEnv_environ */

bool MLton_init(int argc, 
		char **argv,
		void (*loadGlobals)(FILE *file)) {
	char *worldFile;
	int i;
	bool isOriginal;

	Posix_ProcEnv_environ = (CstringArray)environ;
	Real_posInf = HUGE_VAL;
	GC_init(&gcState);
	worldFile = NULL;
	isOriginal = TRUE;
	i = 1;
	if (argc > 1 and (0 == strcmp(argv[1], "@MLton"))) {
		bool done;

		/* process @MLton args */
		i = 2;
		done = FALSE;
		while (!done) {
			if (i == argc)
				usage(argv[0]);
			else {
				string arg;

				arg = argv[i];
				if (0 == strcmp(arg, "fixed-heap")) {
					++i;
					if (i == argc)
						usage(argv[0]);
					gcState.useFixedHeap = TRUE;
					gcState.fromSize =
						stringToBytes(argv[i++]);
				} else if (0 == strcmp(arg, "gc-messages")) {
					++i;
					gcState.messages = TRUE;
				} else if (0 == strcmp(arg, "gc-summary")) {
					++i;
					gcState.summary = TRUE;
				} else if (0 == strcmp(arg, "load-world")) {
					++i;
					isOriginal = FALSE;
					if (i == argc) 
						usage(argv[0]);
					worldFile = argv[i++];
				} else if (0 == strcmp(arg, "max-heap")) {
					fprintf(stderr, "max-heap is currently disabled\n");
					++i;
					if (i == argc) 
						usage(argv[0]);
					gcState.useFixedHeap = FALSE;
					gcState.maxHeapSize =
						stringToBytes(argv[i++]);
				} else if (0 == strcmp(arg, "ram-slop")) {
					++i;
					if (i == argc)
						usage(argv[0]);
					gcState.ramSlop =
						stringToFloat(argv[i++]);
				} else if (0 == strcmp(arg, "--")) {
					++i;
					done = TRUE;
				} else if (i > 1)
					usage(argv[0]);
			        else done = TRUE;
			}
		}
	}
	
	if (isOriginal)
		GC_newWorld(&gcState);
	else
		GC_loadWorld(&gcState, worldFile, loadGlobals);
	
	/* Setup argv and argc that SML sees. */
	/* i is now the index of the first real arg */
	CommandLine_commandName = (uint)(argv[0]);
	CommandLine_argc = argc - i;
	CommandLine_argv = (uint)(argv + i);

	return isOriginal;
}
