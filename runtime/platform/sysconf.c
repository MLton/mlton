size_t GC_pageSize (void) {
  long int pageSize;

  pageSize = sysconf (_SC_PAGESIZE);
  return (size_t)pageSize;
}

uintmax_t GC_physMem (void) {
  size_t pageSize = GC_pageSize ();
  long int physPages;
  uintmax_t physMem;

  physPages = sysconf (_SC_PHYS_PAGES);
  physMem = (uintmax_t)pageSize * (uintmax_t)physPages;
  return physMem;
}
