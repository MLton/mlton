/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline char readChar (FILE *f) {
  char res;
  fread_safe (&res, sizeof(char), 1, f);
  return res;
}

static inline size_t readSize (FILE *f) {
  size_t res;
  fread_safe (&res, sizeof(size_t), 1, f);
  return res;
}

static inline uint32_t readUint32 (FILE *f) {
  uint32_t res;
  fread_safe (&res, sizeof(uint32_t), 1, f);
  return res;
}

static inline uintptr_t readUintptr (FILE *f) {
  uintptr_t res;
  fread_safe (&res, sizeof(uintptr_t), 1, f);
  return res;
}

static inline void writeChar (FILE *f, char c) {
  fwrite_safe (&c, sizeof(char), 1, f);
}

static inline void writeSize (FILE *f, size_t z) {
  fwrite_safe (&z, sizeof(size_t), 1, f);
}

static inline void writeUint32 (FILE *f, uint32_t u) {
  fwrite_safe (&u, sizeof(uint32_t), 1, f);
}

static inline void writeUintptr (FILE *f, uintptr_t u) {
  fwrite_safe (&u, sizeof(uintptr_t), 1, f);
}

static inline void writeString (FILE *f, const char* s) {
  fwrite_safe (s, 1, strlen(s), f);
}

#define BUF_SIZE 81
static inline void writeUint32U (FILE *f, uint32_t u) {
  static char buf[BUF_SIZE];

  snprintf (buf, BUF_SIZE, "%"PRIu32, u);
  writeString (f, buf);
}

static inline void writeUintmaxU (FILE *f, uintmax_t u) {
  static char buf[BUF_SIZE];

  snprintf (buf, BUF_SIZE, "%"PRIuMAX, u);
  writeString (f, buf);
}

static inline void writeUint32X (FILE *f, uint32_t u) {
  static char buf[BUF_SIZE];

  snprintf (buf, BUF_SIZE, "0x%08"PRIx32, u);
  writeString (f, buf);
}

static inline void writeUintmaxX (FILE *f, uintmax_t u) {
  static char buf[BUF_SIZE];

  if (sizeof(uintmax_t) == 4) {
    snprintf (buf, BUF_SIZE, "0x%08"PRIxMAX, u);
  } else if (sizeof(uintmax_t) == 8) {
    snprintf (buf, BUF_SIZE, "0x%016"PRIxMAX, u);
  } else {
    snprintf (buf, BUF_SIZE, "0x%"PRIxMAX, u);
  }
  writeString (f, buf);
}

static inline void writeNewline (FILE *f) {
  writeString (f, "\n");
}

static inline void writeStringWithNewline (FILE *f, const char* s) {
  writeString (f, s);
  writeNewline (f);
}
#undef BUF_SIZE
