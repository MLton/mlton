/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline objptr readObjptr (int fd) {
  objptr res;
  read_safe (fd, &res, sizeof(objptr));
  return res;
}

static inline pointer readPointer (int fd) {
  uintptr_t res;
  read_safe (fd, &res, sizeof(uintptr_t));
  return (pointer)res;
}

static inline void writeObjptr (int fd, objptr op) {
  write_safe (fd, &op, sizeof(objptr));
}

static inline void writePointer (int fd, pointer p) {
  uintptr_t u = (uintptr_t)p;
  write_safe (fd, &u, sizeof(uintptr_t));
}
