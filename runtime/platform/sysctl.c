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
    if (sizeof(size_t) >= sizeof(unsigned long long int)) {
      return (size_t)pageSize;
    } else {
      pageSize = min(pageSize, (unsigned long long int)SIZE_MAX);
      return (size_t)pageSize;
    }
  } else if (len == sizeof(unsigned long int)) {
    unsigned long int pageSize;
    if (-1 == sysctl (mib, 2, &pageSize, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned long int)) {
      return (size_t)pageSize;
    } else {
      pageSize = min(pageSize, (unsigned long int)SIZE_MAX);
      return (size_t)pageSize;
    }
  } else if (len == sizeof(unsigned int)) {
    unsigned int pageSize;
    if (-1 == sysctl (mib, 2, &pageSize, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned int)) {
      return (size_t)pageSize;
    } else {
      pageSize = min(pageSize, (unsigned int)SIZE_MAX);
      return (size_t)pageSize;
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
#if defined(HW_MEMSIZE)
  /* Darwin */
  mib[1] = HW_MEMSIZE;
#elif defined(HW_PHYSMEM64)
  /* NetBSD */
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
    if (sizeof(size_t) >= sizeof(unsigned long long int)) {
      return (size_t)physMem;
    } else {
      physMem = min(physMem, (unsigned long long int)SIZE_MAX);
      return (size_t)physMem;
    }
  } else if (len == sizeof(unsigned long int)) {
    unsigned long int physMem;
    if (-1 == sysctl (mib, 2, &physMem, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned long int)) {
      return (size_t)physMem;
    } else {
      physMem = min(physMem, (unsigned long int)SIZE_MAX);
      return (size_t)physMem;
    }
  } else if (len == sizeof(unsigned int)) {
    unsigned int physMem;
    if (-1 == sysctl (mib, 2, &physMem, &len, NULL, 0))
      diee ("sysctl failed");
    if (sizeof(size_t) >= sizeof(unsigned int)) {
      return (size_t)physMem;
    } else {
      physMem = min(physMem, (unsigned int)SIZE_MAX);
      return (size_t)physMem;
    }
  } else {
    die ("GC_totalRam");
  }
}
