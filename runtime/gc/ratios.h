/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_ratios {
  /* Only use generational GC with copying collection if the ratio of
   * heap size to live data size is below copyGenerational.
   */
  float copyGenerational; 
  /* Minimum live ratio to use copying GC. */
  float copy; 
  float grow; 
  float hashCons;
  /* Desired ratio of heap size to live data. */
  float live; 
  /* Minimum live ratio to us mark-compact GC. */
  float markCompact; 
  /* Only use generational GC with mark-compact collection if the
   * ratio of heap size to live data size is below
   * markCompactGenerational.
   */
  float markCompactGenerational; 
  /* As long as the ratio of bytes live to nursery size is greater
   * than nurseryRatio, use minor GCs.
   */
  float nursery; 
  float threadShrink; 
};
