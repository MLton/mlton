/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool ratiosOk (struct GC_ratios ratios) {
  return 1.0 < ratios.grow
    and 1.0 < ratios.nursery
    and 1.0 < ratios.markCompact
    and ratios.markCompact <= ratios.copy
    and ratios.copy <= ratios.live;
}
