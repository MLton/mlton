#include "gc.h"

/* TERMINATOR is used to separate the human readable messages at the beginning
 * of the world file from the machine readable data.
 */
static const char GC_worldTerminator = '\000';

/* ------------------------------------------------- */
/*                   GC_saveWorld                    */
/* ------------------------------------------------- */

void GC_saveWorld(GC_state s, 
			pointer fileName,
			void (*saveGlobals)(FILE *file)) {
	FILE *file;

	GC_enter(s);
	/* The sopen must happen before the GC, because the GC will invalidate
 	 * the fileName pointer.
	 */
	file = sopen((char*)fileName, "w");
	/* Compact the heap into fromSpace */
	GC_doGC(s, 0, 0);
	fprintf(file, "Heap file created by MLton.\nbase = %x\nfrontier = %x\n",
		(uint)s->base,
		(uint)s->frontier);
 	fputc(GC_worldTerminator, file);
	swriteUint(s->magic, file);
	swriteUint((uint)s->base, file);
	swriteUint((uint)s->frontier, file);
	swriteUint((uint)s->currentThread, file);
	swriteUint((uint)s->signalHandler, file);
 	swrite(s->base, 1, s->frontier - s->base, file);
	(*saveGlobals)(file);
	fclose(file);
	exit(0);
}

/* ------------------------------------------------- */
/*                   translateHeap                   */
/* ------------------------------------------------- */

static void translatePointer(GC_state s, pointer *p) {
	if (1 == s->translateDirection)
		*p += s->translateDiff;
	else
		*p -= s->translateDiff;
}

/* Translate all pointers to the heap from within the stack and the heap for
 * a heap that has moved from s->base == old to s->base.
 */
static void translateHeap(GC_state s, pointer old) {
	if (s->base == old)
		return;
	else if (s->base > old) {
		s->translateDiff = s->base - old;
		s->translateDirection = 1;
	} else {
		s->translateDiff = old - s->base;
		s->translateDirection = -1;
	}
	/* Translate globals and heap. */
	GC_foreachGlobal(s, translatePointer);
	GC_foreachPointerInRange(s, s->base, &s->frontier, translatePointer);
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
	
	GC_initCounters(s);
	file = sopen(fileName, "r");
	until ((c = fgetc(file)) == GC_worldTerminator or EOF == c);
	if (EOF == c) die("Invalid world.");
	magic = sreadUint(file);
	unless (s->magic == magic)
		die("Invalid world: wrong magic number.");
	base = (pointer)sreadUint(file);
	frontier = (pointer)sreadUint(file);
	s->currentThread = (GC_thread)sreadUint(file);
	s->signalHandler = (GC_thread)sreadUint(file);
	heapSize = frontier - base;
	s->bytesLive = heapSize;
       	GC_setHeapParams(s, heapSize);
	GC_fromSpace(s);
	sread(s->base, 1, heapSize, file);
	s->frontier = s->base + heapSize;
	(*loadGlobals)(file);
	unless (EOF == fgetc(file))
		die("Invalid world: junk at end of file.");
	fclose(file);
	/* translateHeap must occur after loading the heap and globals, since it
	 * changes pointers in all of them.
	 */
	translateHeap(s, base);
	GC_setStack(s);
	/* Allocate toSpace.  We at least must do this if s->useFixedHeap. */
	s->toSize = s->fromSize;
	GC_toSpace(s);
	assert(GC_mutatorInvariant(s));
}
