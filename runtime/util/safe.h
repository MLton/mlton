/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline void *calloc_safe (size_t count, size_t size) {
  void *res;
  
  res = calloc (count, size);
  if (NULL == res)
    die ("calloc (%zu, %zu) failed.\n", 
         count, size);
  return res;
}

static inline void close_safe (int fd) {
  int res;

  res = close (fd);
  if (-1 == res)
    diee ("close (%d) failed.\n", fd);
  return;
}

static inline void *malloc_safe (size_t size) {
  void *res;
  
  res = malloc (size);
  if (NULL == res)
    die ("malloc (%zu) failed.\n", size);
  return res;
}

static inline int mkstemp_safe (char *template) {
  int fd;
  
  fd = mkstemp (template);
  if (-1 == fd)
    diee ("mkstemp (%s) failed.\n", template);
  return fd;
}

static inline int open_safe (const char *fileName, int flags, mode_t mode) {
  int res;

  res = open (fileName, flags, mode);
  if (-1 == res)
    diee ("open (%s,_,_) failed.\n", fileName);
  return res;
}

static inline void read_safe (int fd, void *buf, size_t size) {
  ssize_t res;

  if (0 == size) return;
  res = read (fd, buf, size);
  if (res == -1 or (size_t)res != size)
    diee ("read (_, _, _) failed.\n");
}

static inline void unlink_safe (const char *pathname) {
  int res;

  res = unlink (pathname);
  if (-1 == res)
    diee ("unlink (%s) failed.\n", pathname);
  return;
}

static inline void write_safe (int fd, const void *buf, size_t size) {
  ssize_t res;

  if (0 == size) return;
  res = write (fd, buf, size);
  if (res == -1 or (size_t)res != size)
    diee ("write (_, _, _) failed.\n");
}
