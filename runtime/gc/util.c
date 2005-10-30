/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

const char* boolToString (bool b) {
        return b ? "TRUE" : "FALSE";
}

#define BUF_SIZE 81
char* uintmaxToCommaString (uintmax_t n) {
  static char buf1[BUF_SIZE];
  static char buf2[BUF_SIZE];
  static char buf3[BUF_SIZE];
  static char buf4[BUF_SIZE];
  static char buf5[BUF_SIZE];
  static char *bufs[] = {buf1, buf2, buf3, buf4, buf5};
  static int bufIndex = 0;
  static char *buf;
  int i;
  
  buf = bufs[bufIndex++];
  bufIndex %= 5;
  
  i = BUF_SIZE - 1;
  buf[i--] = '\000';
  if (0 == n)
    buf[i--] = '0';
  else {
    while (n > 0) {
      buf[i--] = n % 10 + '0';
      n = n / 10;
      if (i % 4 == 0 and n > 0) buf[i--] = ',';
    }
  }
  return buf + i + 1;
}
