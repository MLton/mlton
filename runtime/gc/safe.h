/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void *calloc_safe (size_t count, size_t size);
void close_safe (int fd);
void *malloc_safe (size_t size);
int mkstemp_safe (char *template);
int open_safe (const char *fileName, int flags, mode_t mode);
void read_safe (int fd, void *buf, size_t size);
void unlink_safe (const char *pathname);
void write_safe (int fd, const void *buf, size_t size);
