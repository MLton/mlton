/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void *calloc_safe (size_t count, size_t size) {
  void *res;
  
  res = calloc (count, size);
  if (NULL == res)
    die ("calloc (%zu, %zu) failed.\n", 
         count, size);
  return res;
}

void *malloc_safe (size_t size) {
  void *res;
  
  res = malloc (size);
  if (NULL == res)
    die ("malloc (%zu) failed.\n", size);
  return res;
}

int mkstemp_safe (char *template) {
  int fd;
  
  fd = mkstemp (template);
  if (-1 == fd)
    diee ("mkstemp (%s) failed.\n", template);
  return fd;
}

int open_safe (const char *fileName, int flags, mode_t mode) {
  int res;

  res = open (fileName, flags, mode);
  if (-1 == res)
    diee ("open (%s,_,_) failed.\n", fileName);
  return res;
}

void close_safe (int fd) {
  int res;

  res = close (fd);
  if (-1 == res)
    diee ("close (%d) failed.\n", fd);
  return;
}

/*
FILE *fopen_safe (char *fileName, char *mode) {
  FILE *file;
  
  file = fopen (fileName, mode);
  if (NULL == file)
    diee ("fopen (%s) failed.\n", fileName);
  return file;
}

void fwrite_safe (const void *data, size_t size, size_t count, FILE *stream) {
  size_t bytes;
  size_t res;

  bytes = size * count;
  if (0 == bytes) return;
  res = fwrite (data, bytes, 1, stream);
  if (1 != res)
    diee ("fwrite (_, _, _, _) failed.\n");
  return;
}

void fclose_safe (FILE *stream) {
  int res;

  res = fclose (stream);
  if (-1 == res)
    diee ("fclose (_) failed.\n");
  return;
}

void fread_safe (void *data, size_t size, size_t count, FILE *stream) {
  size_t bytes;
  size_t res;

  bytes = size * count;
  res = fread (data, bytes, 1, stream);
  if (1 != res)
    diee ("fread (_, _, _, _) failed.\n");
  return;
}
*/

void unlink_safe (const char *pathname) {
  int res;

  res = unlink (pathname);
  if (-1 == res)
    diee ("unlink (%s) failed.\n", pathname);
  return;
}

void read_safe (int fd, void *buf, size_t size) {
  ssize_t res;

  if (0 == size) return;
  res = read (fd, buf, size);
  if (res == -1 or (size_t)res != size)
    diee ("read (_, _, _) failed.\n");
}

void write_safe (int fd, const void *buf, size_t size) {
  ssize_t res;

  if (0 == size) return;
  res = write (fd, buf, size);
  if (res == -1 or (size_t)res != size)
    diee ("write (_, _, _) failed.\n");
}
