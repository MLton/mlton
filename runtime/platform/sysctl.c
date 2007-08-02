size_t GC_pageSize (void) {
  size_t len;
  int mib[2];

  len = 0;
  mib[0] = CTL_HW;
  mib[1] = HW_PAGESIZE;
  if (-1 == sysctl (mib, 2, NULL, &len, NULL, 0))
    diee ("sysctl failed");
  if (len == sizeof(unsigned long long int)) {
    unsigned long long int pageSize;
    if (-1 == sysctl (mib, 2, &pageSize, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned long long int)
        || pageSize <= (unsigned long long int)SIZE_MAX) {
      return (size_t)pageSize;
    } else {
      return (size_t)SIZE_MAX;
    }
  } else if (len == sizeof(unsigned long int)) {
    unsigned long int pageSize;
    if (-1 == sysctl (mib, 2, &pageSize, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned long int)
        || pageSize <= (unsigned long int)SIZE_MAX) {
      return (size_t)pageSize;
    } else {
      return (size_t)SIZE_MAX;
    }
  } else if (len == sizeof(unsigned int)) {
    unsigned int pageSize;
    if (-1 == sysctl (mib, 2, &pageSize, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned int)
        || pageSize <= (unsigned int)SIZE_MAX) {
      return (size_t)pageSize;
    } else {
      return (size_t)SIZE_MAX;
    }
  } else {
    die ("GC_pageSize");
  }
}

size_t GC_totalRam (void) {
  size_t len;
  int mib[2];

  len = 0;
  mib[0] = CTL_HW;
#ifdef HW_PHYSMEM64
  mib[1] = HW_PHYSMEM64;
#else
  mib[1] = HW_PHYSMEM;
#endif
  if (-1 == sysctl (mib, 2, NULL, &len, NULL, 0))
    diee ("sysctl failed");
  if (len == sizeof(unsigned long long int)) {
    unsigned long long int physMem;
    if (-1 == sysctl (mib, 2, &physMem, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned long long int)
        || physMem <= (unsigned long long int)SIZE_MAX) {
      return (size_t)physMem;
    } else {
      return (size_t)SIZE_MAX;
    }
  } else if (len == sizeof(unsigned long int)) {
    unsigned long int physMem;
    if (-1 == sysctl (mib, 2, &physMem, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned long int)
        || physMem <= (unsigned long int)SIZE_MAX) {
      return (size_t)physMem;
    } else {
      return (size_t)SIZE_MAX;
    }
  } else if (len == sizeof(unsigned int)) {
    unsigned int physMem;
    if (-1 == sysctl (mib, 2, &physMem, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned int)
        || physMem <= (unsigned int)SIZE_MAX) {
      return (size_t)physMem;
    } else {
      return (size_t)SIZE_MAX;
    }
  } else {
    die ("GC_totalRam");
  }
}
