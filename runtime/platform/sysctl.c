size_t GC_totalRam (void) {
  int mem;
  size_t len;
  int mib[2];
        
  mib[0] = CTL_HW;
  mib[1] = HW_PHYSMEM;
  len = sizeof(mem);
  if (-1 == sysctl (mib, 2, &mem, &len, NULL, 0))
    diee ("sysctl failed");
  return (size_t)mem;
}
