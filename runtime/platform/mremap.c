#define GC_SMALLEST_PAGESIZE 4096
/* #define DEBUG_MREMAP 1 */

/* Used to allocate at the head and tail of an existing allocation */
static void *GC_extendHead (void *base, size_t length);
static void *GC_extendTail (void *base, size_t length);
  
void *GC_mremap (void *base, size_t oldLength, size_t newLength) {
  static void*  cacheAddress = 0;
  static size_t cacheOldLength = 0;
  static size_t cacheNewLength = 0;
  static size_t cacheTailSize;
  static size_t cacheHeadSize;
   
  void* tail;
  void* head;
  void* alloc;
  size_t growth, bsLow, bsHigh, bsTry;

#ifdef DEBUG_MREMAP  
  fprintf(stderr, "remap(%08X, %d, %d)\n", (size_t)base, oldLength, newLength);
  fflush(stderr);
#endif
  
  if (newLength == oldLength)
    return base;
  if (newLength < oldLength) {
    GC_release((char*)base + newLength, oldLength-newLength);
    return base;
  }
  
  /* Prefer a moving remap -> it results in less mmapped regions */
  alloc = GC_mmapAnon(0, newLength);
  if (alloc != (void*)-1) {
    memcpy(alloc, base, oldLength);
    GC_release(base, oldLength);
    return alloc;
  }
  
  growth = newLength-oldLength;
  
  if (cacheAddress   == base      && 
      cacheOldLength == oldLength &&
      cacheNewLength >  newLength) /* cache only during backoff */
    goto GC_mremap_cached;
  
  /* Start probing for the available tail length */
  bsLow = 0;
  bsHigh = (growth+GC_SMALLEST_PAGESIZE-1)/GC_SMALLEST_PAGESIZE;
  /* Round bsHigh to a power of two -> allocation works with all page sizes */
  for (bsTry = 1; bsTry <= bsHigh; bsTry += bsTry) { }
  bsHigh = bsTry;
  while (bsHigh - bsLow > 1) {
    bsTry = (bsHigh + bsLow)/2;
    tail = (char*)base + oldLength;
    alloc = GC_extendTail(tail, bsTry*GC_SMALLEST_PAGESIZE);
    
    if (tail == alloc) {
      GC_release(alloc, bsTry*GC_SMALLEST_PAGESIZE);
      bsLow = bsTry;
    } else {
      if (alloc != (void*)-1)
        GC_release(alloc, bsTry*GC_SMALLEST_PAGESIZE);
      bsHigh = bsTry;
    }
  }
  cacheTailSize = bsLow*GC_SMALLEST_PAGESIZE;
  
  /* Start probing for the available head length */
  bsLow = 0;
  bsHigh = (growth+GC_SMALLEST_PAGESIZE-1)/GC_SMALLEST_PAGESIZE;
  /* Round bsHigh to a power of two -> allocation works with all page sizes */
  for (bsTry = 1; bsTry <= bsHigh; bsTry += bsTry) { }
  bsHigh = bsTry;
  while (bsHigh - bsLow > 1) {
    bsTry = (bsHigh + bsLow)/2;
    head = (char*)base - bsTry*GC_SMALLEST_PAGESIZE;
    alloc = GC_extendHead(head, bsTry*GC_SMALLEST_PAGESIZE);
    
    if (head == alloc) {
      GC_release(alloc, bsTry*GC_SMALLEST_PAGESIZE);
      bsLow = bsTry;
    } else {
      if (alloc != (void*)-1)
        GC_release(alloc, bsTry*GC_SMALLEST_PAGESIZE);
      bsHigh = bsTry;
    }
  }
  cacheHeadSize = bsLow*GC_SMALLEST_PAGESIZE;
  
#ifdef DEBUG_MREMAP  
  fprintf(stderr, "Expansion detects: %10d/%10d/%10d = %10d\n", 
   cacheHeadSize, oldLength, cacheTailSize,
   cacheHeadSize+ oldLength+ cacheTailSize);
  fflush(stderr);
#endif
  
  cacheAddress = base;
  cacheOldLength = oldLength;
  
GC_mremap_cached:
  cacheNewLength = newLength;
  
  /* Is there enough free space? */
  if (cacheTailSize + cacheHeadSize < growth) {
    return (void*)-1;
  }
  
#ifdef DEBUG_MREMAP  
  fprintf(stderr, "Expansion attempts %d bytes\n", newLength);
  fflush(stderr);
#endif
  
  if (growth <= cacheTailSize) {
    tail = (char*)base + oldLength;
    alloc = GC_extendTail(tail, growth);
    if (alloc != tail) {
      if (alloc != (void*)-1)
        GC_release(alloc, growth);
      return (void*)-1;
    }
    return base;
  }
  
  if (cacheTailSize > 0) {
    tail = (char*)base + oldLength;
    alloc = GC_extendTail(tail, cacheTailSize);
    if (alloc != tail) {
      if (alloc != (void*)-1)
        GC_release(alloc, cacheTailSize);
      return (void*)-1;
    }
  } else {
    tail = 0; /* quell warning */
  }
  
  head = (char*)base - (growth-cacheTailSize);
  alloc = GC_extendHead(head, growth-cacheTailSize);
  if (alloc != head) {
    if (alloc != (void*)-1)
      GC_release(alloc, growth-cacheTailSize);
    if (cacheTailSize > 0)
      GC_release(tail, cacheTailSize);
    return (void*)-1;
  }
  
  memmove(head, base, oldLength);
  return head;
}
