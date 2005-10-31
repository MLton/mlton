/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void rusageZero (struct rusage *ru);
void rusagePlusMax (struct rusage *ru1,
                    struct rusage *ru2,
                    struct rusage *ru);
void rusageMinusMax (struct rusage *ru1,
                     struct rusage *ru2,
                     struct rusage *ru);
uintmax_t rusageTime (struct rusage *ru);
uintmax_t currentTime (void);
void startTiming (struct rusage *ru_start);
uintmax_t stopTiming (struct rusage *ru_start, struct rusage *ru_gc);
