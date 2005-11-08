/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void loadWorldFromFD (GC_state s, int fd) {
  uint32_t magic;
  pointer start;
        
  if (DEBUG_WORLD)
    fprintf (stderr, "loadWorldFromFD (%d)\n", fd);
  until (readChar (fd) == '\000') ;
  magic = readUint32 (fd);
  unless (s->magic == magic)
    die ("Invalid world: wrong magic number.");
  start = readPointer (fd);
  s->heap.oldGenSize = readSize (fd);
  s->atomicState = readUint32 (fd);
  s->callFromCHandlerThread = readObjptr (fd); 
  s->currentThread = readObjptr (fd); 
  s->signalHandlerThread = readObjptr (fd); 
  createHeap (s, &s->heap, 
              sizeofHeapDesired (s, s->heap.oldGenSize, 0), 
              s->heap.oldGenSize);
  createCardMapAndCrossMap (s); 
  read_safe (fd, s->heap.start, s->heap.oldGenSize);
  (*s->loadGlobals) (fd);
  // unless (EOF == fgetc (file))
  //  die ("Invalid world: junk at end of file.");
  /* translateHeap must occur after loading the heap and globals,
   * since it changes pointers in all of them.
   */
  translateHeap (s, start, s->heap.start, s->heap.oldGenSize);
  setGCStateCurrentHeap (s, 0, 0); 
  setGCStateCurrentThreadAndStack (s); 
}

void loadWorldFromFileName (GC_state s, char *fileName) {
  int fd;
        
  if (DEBUG_WORLD)
    fprintf (stderr, "loadWorldFromFileName (%s)\n", fileName);
  fd = open_safe (fileName, O_RDONLY, 0); 
  loadWorldFromFD (s, fd);
  close_safe (fd); 
}

void saveWorldToFD (GC_state s, int fd) {
  char buf[80];

  if (DEBUG_WORLD)
    fprintf (stderr, "saveWorld (%d).\n", fd);
  /* Compact the heap. */
  performGC (s, 0, 0, TRUE, TRUE);
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
}

void GC_saveWorld (GC_state s, int fd) {
  enter (s);
  saveWorldToFD (s, fd);
  leave (s);
}
