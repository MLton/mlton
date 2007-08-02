size_t GC_pageSize (void) {
  long int pageSize;

  pageSize = sysconf (_SC_PAGESIZE);
  return (size_t)pageSize;
}

size_t GC_totalRam (void) {
  size_t pageSize = GC_pageSize ();
  long int physPages;
  uintmax_t totalRam;

  physPages = sysconf (_SC_PHYS_PAGES);
  totalRam = (uintmax_t)pageSize * (uintmax_t)physPages;
  if (sizeof(size_t) >= sizeof(uintmax_t)) {
    return (uintmax_t)totalRam;
  } else {
    totalRam = min(totalRam, (uintmax_t)SIZE_MAX);
    return (size_t)totalRam;
  }
}
