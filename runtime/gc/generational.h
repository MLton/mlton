/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_generationalInfo {
  uint8_t *cardMap;
  uint8_t *cardMapAbsolute;
  size_t cardMapSize;
  size_t cardSize;
  uint8_t *crossMap;
  size_t crossMapSize;
  /* crossMapValidEnd is the size of the prefix of the old generation
   * for which the crossMap is valid.
   */
  size_t crossMapValidSize;
  /*Bool*/bool mutatorMarksCards;
};
