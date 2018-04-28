/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void rusageZero (struct rusage *ru) {
  memset (ru, 0, sizeof (*ru));
}

void rusagePlusMax (struct rusage *ru1,
                    struct rusage *ru2,
                    struct rusage *ru) {
  const int       million = 1000000;
  time_t          sec;
  suseconds_t     usec;

  sec = ru1->ru_utime.tv_sec + ru2->ru_utime.tv_sec;
  usec = ru1->ru_utime.tv_usec + ru2->ru_utime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_utime.tv_sec = sec;
  ru->ru_utime.tv_usec = usec;

  sec = ru1->ru_stime.tv_sec + ru2->ru_stime.tv_sec;
  usec = ru1->ru_stime.tv_usec + ru2->ru_stime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_stime.tv_sec = sec;
  ru->ru_stime.tv_usec = usec;
}

void rusageMinusMax (struct rusage *ru1,
                     struct rusage *ru2,
                     struct rusage *ru) {
  const int       million = 1000000;
  time_t          sec;
  suseconds_t     usec;

  sec = (ru1->ru_utime.tv_sec - ru2->ru_utime.tv_sec) - 1;
  usec = ru1->ru_utime.tv_usec + million - ru2->ru_utime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_utime.tv_sec = sec;
  ru->ru_utime.tv_usec = usec;

  sec = (ru1->ru_stime.tv_sec - ru2->ru_stime.tv_sec) - 1;
  usec = ru1->ru_stime.tv_usec + million - ru2->ru_stime.tv_usec;
  sec += (usec / million);
  usec %= million;
  ru->ru_stime.tv_sec = sec;
  ru->ru_stime.tv_usec = usec;
}

uintmax_t rusageTime (struct rusage *ru) {
  uintmax_t result;

  result = 0;
  result += 1000 * (uintmax_t)ru->ru_utime.tv_sec;
  result += 1000 * (uintmax_t)ru->ru_stime.tv_sec;
  result += (uintmax_t)ru->ru_utime.tv_usec / 1000;
  result += (uintmax_t)ru->ru_stime.tv_usec / 1000;
  return result;
}

void startTiming (struct rusage *ru_start) {
  getrusage (RUSAGE_SELF, ru_start);
}

/* Accumulate and return time as number of milliseconds. */
uintmax_t stopTiming (struct rusage *ru_start, struct rusage *ru_acc) {
  struct rusage ru_finish, ru_total;

  getrusage (RUSAGE_SELF, &ru_finish);
  rusageMinusMax (&ru_finish, ru_start, &ru_total);
  rusagePlusMax (ru_acc, &ru_total, ru_acc);
  return rusageTime (&ru_total);
}
