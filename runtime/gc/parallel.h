
#if (defined (MLTON_GC_INTERNAL_BASIS))

void Parallel_init (void);

void Parallel_lock (Int32);
void Parallel_unlock (Int32);

Int32 Parallel_processorNumber (void);
Int32 Parallel_numberOfProcessors (void);
Word64 Parallel_maxBytesLive (void);
void Parallel_resetBytesLive (void);

Int32 Parallel_fetchAndAdd (pointer p, Int32 v);
bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

