/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void loadWorldFromFILE (GC_state s, FILE *f) {
  uint32_t magic;
  pointer start;

  if (DEBUG_WORLD)
    fprintf (stderr, "loadWorldFromFILE\n");
  until (readChar (f) == '\000') ;
  magic = readUint32 (f);
  unless (s->magic == magic)
    die ("Invalid world: wrong magic number.");
  start = readPointer (f);
  s->heap.oldGenSize = readSize (f);
  s->atomicState = readUint32 (f);
  s->callFromCHandlerThread = readObjptr (f);
  s->currentThread = readObjptr (f);
  s->signalHandlerThread = readObjptr (f);
  createHeap (s, &s->heap,
              sizeofHeapDesired (s, s->heap.oldGenSize, 0),
              s->heap.oldGenSize);
  setCardMapAndCrossMap (s);
  fread_safe (s->heap.start, 1, s->heap.oldGenSize, f);
  if ((*(s->loadGlobals)) (f) != 0) diee("couldn't load globals");
  // unless (EOF == fgetc (file))
  //  die ("Invalid world: junk at end of file.");
  /* translateHeap must occur after loading the heap and globals,
   * since it changes pointers in all of them.
   */
  translateHeap (s, start, s->heap.start, s->heap.oldGenSize);
  setGCStateCurrentHeap (s, 0, 0);
  setGCStateCurrentThreadAndStack (s);
}

void loadWorldFromFileName (GC_state s, const char *fileName) {
  FILE *f;

  if (DEBUG_WORLD)
    fprintf (stderr, "loadWorldFromFileName (%s)\n", fileName);
  f = fopen_safe (fileName, "rb");
  loadWorldFromFILE (s, f);
  fclose_safe (f);
}

/* Don't use 'safe' functions, because we don't want the ML program to die.
 * Instead, check return values, and propogate them up to SML for an exception.
 */
int saveWorldToFILE (GC_state s, FILE *f) {
  char buf[128];
  size_t len;

  if (DEBUG_WORLD)
    fprintf (stderr, "saveWorldToFILE\n");
  /* Compact the heap. */
  performGC (s, 0, 0, TRUE, TRUE);
  snprintf (buf, cardof(buf),
            "Heap file created by MLton.\nheap.start = "FMTPTR"\nbytesLive = %"PRIuMAX"\n",
            (uintptr_t)s->heap.start,
            (uintmax_t)s->lastMajorStatistics.bytesLive);
  len = strlen(buf) + 1; /* +1 to get the '\000' */

  if (fwrite (buf, 1, len, f) != len) return -1;
  if (fwrite (&s->magic, sizeof(uint32_t), 1, f) != 1) return -1;
  if (fwrite (&s->heap.start, sizeof(uintptr_t), 1, f) != 1) return -1;
  if (fwrite (&s->heap.oldGenSize, sizeof(size_t), 1, f) != 1) return -1;

  /* atomicState must be saved in the heap, because the saveWorld may
   * be run in the context of a critical section, which will expect to
   * be in the same context when it is restored.
   */
  if (fwrite (&s->atomicState, sizeof(uint32_t), 1, f) != 1) return -1;
  if (fwrite (&s->callFromCHandlerThread, sizeof(objptr), 1, f) != 1) return -1;
  if (fwrite (&s->currentThread, sizeof(objptr), 1, f) != 1) return -1;
  if (fwrite (&s->signalHandlerThread, sizeof(objptr), 1, f) != 1) return -1;

  if (fwrite (s->heap.start, 1, s->heap.oldGenSize, f) != s->heap.oldGenSize)
    return -1;
  if ((*(s->saveGlobals)) (f) != 0)
    return -1;
  return 0;
}

void GC_saveWorld (GC_state s, NullString8_t fileName) {
  FILE *f;

  enter (s);
  f = fopen ((const char*)fileName, "wb");
  if (f == 0) {
    s->saveWorldStatus = false;
    goto done;
  }
  if (saveWorldToFILE (s, f) != 0) {
    s->saveWorldStatus = false;
    goto done;
  }
  if (fclose (f) != 0) {
    s->saveWorldStatus = false;
    goto done;
  }

  s->saveWorldStatus = true;
done:
  leave (s);
  return;
}

C_Errno_t(Bool_t) GC_getSaveWorldStatus (GC_state s) {
  return (Bool_t)(s->saveWorldStatus);
}
