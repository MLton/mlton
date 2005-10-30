/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static void loadWorld (GC_state s, char *fileName) {
  int fd;
  uint32_t magic;
  pointer start;
        
  if (DEBUG_WORLD)
    fprintf (stderr, "loadWorld (%s)\n", fileName);
  fd = open_safe (fileName, O_RDONLY, 0); 
  until (readChar (fd) == '\000') ;
  magic = readUint32 (fd);
  unless (s->magic == magic)
    die ("Invalid world: wrong magic number.");
  start = readPointer (fd);
  s->heap.oldGenSize = readSize (fd);
  s->atomicState = readUint32 (fd);
  s->callFromCHandlerThread =  readObjptr (fd); 
  s->currentThread = readObjptr (fd); 
  s->signalHandlerThread = readObjptr (fd); 
  heapCreate (s, &s->heap, 
              heapDesiredSize (s, s->heap.oldGenSize, 0), 
              s->heap.oldGenSize);
  createCardMapAndCrossMap (s); 
  read_safe (fd, s->heap.start, s->heap.oldGenSize);
  (*s->loadGlobals) (fd);
  // unless (EOF == fgetc (file))
  //  die ("Invalid world: junk at end of file.");
  close_safe (fd); 
  /* translateHeap must occur after loading the heap and globals,
   * since it changes pointers in all of them.
   */
  translateHeap (s, start, s->heap.start, s->heap.oldGenSize);
  heapSetNursery (s, 0, 0); 
  setCurrentStack (s); 
}

static void saveWorld (GC_state s, int fd) {
  char buf[80];

  if (DEBUG_WORLD)
    fprintf (stderr, "GC_saveWorld (%d).\n", fd);
  enter (s);
  /* Compact the heap. */
  doGC (s, 0, 0, TRUE, TRUE);
  sprintf (buf,
           "Heap file created by MLton.\nheap.start = "FMTPTR"\nbytesLive = %zu\n",
           (uintptr_t)s->heap.start, 
           s->lastMajorStatistics.bytesLive);
  write_safe (fd, buf, 1 + strlen(buf)); /* +1 to get the '\000' */
  writeUint32 (fd, s->magic);
  writePointer (fd, s->heap.start);
  writeSize (fd, s->heap.oldGenSize);
  /* atomicState must be saved in the heap, because the saveWorld may
   * be run in the context of a critical section, which will expect to
   * be in the same context when it is restored.
   */
  writeUint32 (fd, s->atomicState);
  writeObjptr (fd, s->callFromCHandlerThread);
  writeObjptr (fd, s->currentThread);
  writeObjptr (fd, s->signalHandlerThread);
  write_safe (fd, s->heap.start, s->heap.oldGenSize);
  (*s->saveGlobals) (fd);
  leave (s);
}
