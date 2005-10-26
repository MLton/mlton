/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


static void writeString (int fd, char* s) {
  write_safe (fd, s, strlen(s));
}

static void writeUint32U (int fd, uint32_t u) {
  char buf[(UINT32_MAX / 10) + 2];

  sprintf (buf, "%"PRIu32, u);
  writeString (fd, buf);
}

static void writeUintmaxU (int fd, uintmax_t u) {
  // char buf[(UINTMAX_MAX / 10) + 2];
  char buf[20];

  sprintf (buf, "%"PRIuMAX, u);
  writeString (fd, buf);
}

static void writeUint32X (int fd, uint32_t u) {
  char buf[5 + (UINT32_MAX / 16) + 2];
  
  sprintf (buf, "0x%08"PRIx32, u);
  writeString (fd, buf);
}

static inline void writeNewline (int fd) {
        writeString (fd, "\n");
}
