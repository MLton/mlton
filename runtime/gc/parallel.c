
#include <pthread.h>
#include <time.h>
#include "platform.h"

/* num of holding thread or -1 if no one*/
volatile int32_t *Parallel_mutexes;

void Parallel_init (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  if (!Proc_isInitialized (s)) {
    Parallel_mutexes = (int32_t *) malloc (s->numberOfProcs * sizeof (int32_t));

    /* Set up call-back state in each worker thread */
    /* XXX hack copy the call-from-c-handler into the worker threads 
       assumes this is called by the primary thread */
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      s->procStates[proc].callFromCHandlerThread = pointerToObjptr(
        GC_copyThread (s, objptrToPointer(s->callFromCHandlerThread,
                                          s->heap->start)),
        s->heap->start);

      Parallel_mutexes[proc] = -1;
    }
    /* Now wake them up! */
    Proc_signalInitialization (s);
  }
}

Int32 Parallel_processorNumber (void) {
  GC_state s = pthread_getspecific (gcstate_key);  
  return Proc_processorNumber (s);
}

Int32 Parallel_numberOfProcessors (void) {
  GC_state s = pthread_getspecific (gcstate_key);  
  return s->numberOfProcs;
}


Word64 Parallel_maxBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return (uint64_t)s->cumulativeStatistics->maxBytesLiveSinceReset;
}

void Parallel_resetBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->cumulativeStatistics->maxBytesLiveSinceReset = 0;
}


static void maybeWaitForGC (GC_state s) {
  if (Proc_threadInSection (s)) {
    //fprintf (stderr, "waiting for gc [%d]\n", Proc_processorNumber (s));

    /* XXX hack? */
    ENTER0 (s);
    LEAVE0 (s);
  }
}

//struct rusage ru_lock;

void Parallel_lock (Int32 p) {
  GC_state s = pthread_getspecific (gcstate_key);
  int32_t myNumber = Proc_processorNumber (s);

  //fprintf (stderr, "lock\n");

  /* 
  if (needGCTime (s))
    startTiming (&ru_lock);
  */

  do {
  AGAIN:
    maybeWaitForGC (s);
    if (Parallel_mutexes[p] >= 0)
      goto AGAIN;
  } while (not __sync_bool_compare_and_swap (&Parallel_mutexes[p],
                                             -1,
                                             myNumber));
  /* 
  if (needGCTime (s))
    stopTiming (&ru_lock, &s->cumulativeStatistics->ru_lock);
  */
}

void Parallel_unlock (Int32 p) {
  GC_state s = pthread_getspecific (gcstate_key);
  int32_t myNumber = Proc_processorNumber (s);

  //fprintf (stderr, "unlock %d\n", Parallel_holdingMutex);

  if (not __sync_bool_compare_and_swap (&Parallel_mutexes[p],
                                        myNumber,
                                        -1)) {
    fprintf (stderr, "can't unlock if you don't hold the lock\n");
  }
}

Int32 Parallel_fetchAndAdd (pointer p, Int32 v) {
  //fprintf (stderr, "fetchAndAdd\n");
  /*
  Int32 res = __sync_fetch_and_add ((Int32 *)p, v);
  asm volatile ("mfence");
  */

  asm volatile ("lock; xaddl %0,%1"
                : "+q" (v) // output
                : "m" (*p) // input
                : "memory"); // clobbered
  //  asm volatile ("mfence");

  return v;
}

bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new) {
  return __sync_bool_compare_and_swap ((Int32 *)p, old, new);
}
