#include "platform.h"

#include "getrusage.c"
#include "getText.c"
#include "mkdir2.c"
#include "mmap.c"
#include "ssmmap.c"
#include "totalRam.sysconf.c"

void decommit (void *base, size_t length) {
	smunmap (base, length);
}

/* On Solaris 5.7, MAP_ANON causes EINVAL and mmap requires a file descriptor. */
void *mmapAnon (void *start, size_t length) {
	static int fd = -1;

	if (-1 == fd)
		fd = open ("/dev/zero", O_RDONLY);
	return mmap (start, length, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
}

void release (void *base, size_t length) {
	smunmap (base, length);
}

/* This implementation of setenv has a space leak, but I don't see how to avoid 
 * it, since the specification of putenv is that it uses the memory for its arg.
 */
int setenv (const char *name, const char *value, int overwrite) {
	char *b;

	b = malloc (strlen (name) + strlen (value) + 2 /* = and \000 */);
	sprintf (b, "%s=%s", name, value);
	return putenv (b);
}

void showMem () {
	static char buffer[256];
	sprintf (buffer, "pmap %d\n", (int)(getpid ()));
	system (buffer);
}
