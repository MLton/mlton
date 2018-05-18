/* Copyright (C) 2012,2013 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "util.h"

const char* boolToString (bool b) {
  return b ? "TRUE" : "FALSE";
}

#define BUF_SIZE 64
char* intmaxToCommaString (intmax_t n) {
  static char buf1[BUF_SIZE];
  static char buf2[BUF_SIZE];
  static char buf3[BUF_SIZE];
  static char buf4[BUF_SIZE];
  static char buf5[BUF_SIZE];
  static char *bufs[] = {buf1, buf2, buf3, buf4, buf5};
  static int bufIndex = 0;
  static char *buf;
  char tmp[BUF_SIZE];
  int i, j, k, l;

  buf = bufs[bufIndex++];
  bufIndex %= 5;

  l = snprintf(tmp, BUF_SIZE, "%"PRIdMAX, n);
  if (tmp[0] == '-') {
    buf[0] = '-';
    i = 1;
    j = 1;
    k = (l - 1) % 3;
  } else {
    i = 0;
    j = 0;
    k = l % 3;
  }
  if (k == 0) {
    k = 3;
  }
  buf[j++] = tmp[i++];
  k--;
  while (tmp[i] != '\000') {
    if (k == 0) {
      buf[j++] = ',';
      k = 3;
    }
    buf[j++] = tmp[i++];
    k--;
  }
  buf[j] = '\000';

  return buf;
}

char* uintmaxToCommaString (uintmax_t n) {
  static char buf1[BUF_SIZE];
  static char buf2[BUF_SIZE];
  static char buf3[BUF_SIZE];
  static char buf4[BUF_SIZE];
  static char buf5[BUF_SIZE];
  static char *bufs[] = {buf1, buf2, buf3, buf4, buf5};
  static int bufIndex = 0;
  static char *buf;
  char tmp[BUF_SIZE];
  int i, j, k, l;

  buf = bufs[bufIndex++];
  bufIndex %= 5;

  l = snprintf(tmp, BUF_SIZE, "%"PRIuMAX, n);
  i = 0;
  j = 0;
  k = l % 3;
  if (k == 0) {
    k = 3;
  }
  buf[j++] = tmp[i++];
  k--;
  while (tmp[i] != '\000') {
    if (k == 0) {
      buf[j++] = ',';
      k = 3;
    }
    buf[j++] = tmp[i++];
    k--;
  }
  buf[j] = '\000';

  return buf;
}
#undef BUF_SIZE
