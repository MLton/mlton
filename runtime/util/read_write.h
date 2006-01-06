/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline char readChar (int fd) {
  char res;
  read_safe (fd, &res, sizeof(char));
  return res;
}

static inline size_t readSize (int fd) {
  size_t res;
  read_safe (fd, &res, sizeof(size_t));
  return res;
}

static inline uint32_t readUint32 (int fd) {
  uint32_t res;
  read_safe (fd, &res, sizeof(uint32_t));
  return res;
}

static inline uintptr_t readUintptr (int fd) {
  uintptr_t res;
  read_safe (fd, &res, sizeof(uintptr_t));
  return res;
}

static inline void writeChar (int fd, char c) {
  write_safe (fd, &c, sizeof(char));
}

static inline void writeSize (int fd, size_t z) {
  write_safe (fd, &z, sizeof(size_t));
}

static inline void writeUint32 (int fd, uint32_t u) {
  write_safe (fd, &u, sizeof(uint32_t));
}

static inline void writeUintptr (int fd, uintptr_t u) {
  write_safe (fd, &u, sizeof(uintptr_t));
}

static inline void writeString (int fd, char* s) {
  write_safe (fd, s, strlen(s));
}

#define BUF_SIZE 81
static inline void writeUint32U (int fd, uint32_t u) {
  static char buf[BUF_SIZE];

  sprintf (buf, "%"PRIu32, u);
  writeString (fd, buf);
}

static inline void writeUintmaxU (int fd, uintmax_t u) {
  static char buf[BUF_SIZE];

  sprintf (buf, "%"PRIuMAX, u);
  writeString (fd, buf);
}

static inline void writeUint32X (int fd, uint32_t u) {
  static char buf[BUF_SIZE];
  
  sprintf (buf, "0x%08"PRIx32, u);
  writeString (fd, buf);
}

static inline void writeUintmaxX (int fd, uintmax_t u) {
  static char buf[BUF_SIZE];

  if (sizeof(uintmax_t) == 4) {
    sprintf (buf, "0x%08"PRIxMAX, u);
  } else if (sizeof(uintmax_t) == 8) {
    sprintf (buf, "0x%016"PRIxMAX, u);
  } else {
    sprintf (buf, "0x"PRIxMAX, u);
  }
  writeString (fd, buf);
}

static inline void writeNewline (int fd) {
  writeString (fd, "\n");
}
#undef BUF_SIZE
