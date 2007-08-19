/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool isBigEndian(void) {
  union {
    uint16_t x;
    uint8_t y;
  } z;

  /* gcc optimizes the following code to just return the result. */
  z.x = 0xABCDU;
  if (z.y == 0xAB) return TRUE; /* big endian */
  if (z.y == 0xCD) return FALSE; /* little endian */
  die ("Could not detect endian --- neither big nor little!\n");
  return 0;
}

static inline bool isLittleEndian(void) {
  return not (isBigEndian());
}
