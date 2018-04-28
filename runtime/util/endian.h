/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool isBigEndian(void) {
  uint16_t x = 0xABCDU;
  uint8_t y = 0;
  /* gcc optimizes the following code to just return the result. */
  memcpy(&y, &x, sizeof(uint8_t));
  if (y == 0xAB) return TRUE; /* big endian */
  if (y == 0xCD) return FALSE; /* little endian */
  die ("Could not detect endian --- neither big nor little!\n");
  // return 0;
}

static inline bool isLittleEndian(void) {
  return not (isBigEndian());
}
