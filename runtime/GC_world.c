#include <string.h>
#include "gc.h"

/* TERMINATOR is used to separate the human readable messages at the beginning
 * of the world file from the machine readable data.
 */
static const char GC_worldTerminator = '\000';

/* ------------------------------------------------- */
/*                   GC_saveWorld                    */
/* ------------------------------------------------- */

void GC_saveWorld(GC_state s, int fd, void (*saveGlobals)(int fd)) {
	char buf[80];

	GC_enter(s);
	/* Compact the heap into fromSpace */
	GC_doGC(s, 0, 0);
	sprintf(buf,
		"Heap file created by MLton.\nbase = %x\nfrontier = %x\n",
		(uint)s->base,
		(uint)s->frontier);
	swrite(fd, buf, 1 + strlen(buf)); /* +1 to get the '\000' */
	swriteUint(fd, s->magic);
	swriteUint(fd, (uint)s->base);
	swriteUint(fd, (uint)s->frontier);
	swriteUint(fd, (uint)s->currentThread);
	swriteUint(fd, (uint)s->signalHandler);
 	swrite(fd, s->base, s->frontier - s->base);
	(*saveGlobals)(fd);
	GC_leave(s);
}

/* ------------------------------------------------- */
/*                   GC_loadWorld                    */
/* ------------------------------------------------- */

void GC_loadWorld(GC_state s, 
			char *fileName,
			void (*loadGlobals)(FILE *file)) {
	FILE *file;
	uint heapSize, magic;
	pointer base, frontier;
	char c;
	
	file = sfopen(fileName, "r");
	until ((c = fgetc(file)) == GC_worldTerminator or EOF == c);
	if (EOF == c) die("Invalid world.");
	magic = sfreadUint(file);
	unless (s->magic == magic)
		die("Invalid world: wrong magic number.");
	base = (pointer)sfreadUint(file);
	frontier = (pointer)sfreadUint(file);
	s->currentThread = (GC_thread)sfreadUint(file);
	s->signalHandler = (GC_thread)sfreadUint(file);
	heapSize = frontier - base;
	s->bytesLive = heapSize;
       	GC_setHeapParams(s, heapSize);
	GC_fromSpace(s);
	sfread(s->base, 1, heapSize, file);
	s->frontier = s->base + heapSize;
	(*loadGlobals)(file);
	unless (EOF == fgetc(file))
		die("Invalid world: junk at end of file.");
	fclose(file);
	/* translateHeap must occur after loading the heap and globals, since it
	 * changes pointers in all of them.
	 */
	GC_translateHeap(s, base, s->base, heapSize);
	GC_setStack(s);
	/* Allocate toSpace.  We at least must do this if s->useFixedHeap. */
	s->toSize = s->fromSize;
	GC_toSpace(s);
	assert(GC_mutatorInvariant(s));
}
