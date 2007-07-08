size_t GC_pageSize (void) {
  int pageSize;
  size_t len;
  int mib[2];

  mib[0] = CTL_HW;
  mib[1] = HW_PAGESIZE;
  len = sizeof(pageSize);
  if (-1 == sysctl (mib, 2, &pageSize, &len, NULL, 0))
    diee ("sysctl failed");
  return (size_t)pageSize;
}

size_t GC_totalRam (void) {
  size_t physMem;
  size_t len;
  int mib[2];

  mib[0] = CTL_HW;
  mib[1] = HW_PHYSMEM;
  len = sizeof(physMem);
  if (-1 == sysctl (mib, 2, &physMem, &len, NULL, 0))
    diee ("sysctl failed");
  return (size_t)physMem;
}
