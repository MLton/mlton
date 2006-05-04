size_t GC_pageSize (void) {
  long int tmp;

  tmp = sysconf (_SC_PAGESIZE);
  return (size_t)tmp;
}

size_t GC_totalRam (void) {
  size_t pageSize = GC_pageSize ();
  long int tmp;

  tmp = sysconf (_SC_PHYS_PAGES);
  return pageSize * (size_t)tmp;
}

size_t GC_availRam (void) {
  size_t pageSize = GC_pageSize ();
  long int tmp;

  tmp = sysconf (_SC_AVPHYS_PAGES);
  return pageSize * (size_t)tmp;
}
