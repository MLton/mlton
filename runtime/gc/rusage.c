/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void rusageZero (struct rusage *ru) {
  memset (ru, 0, sizeof (*ru));
}

void rusagePlusMax (struct rusage *ru1,
                    struct rusage *ru2,
                    struct rusage *ru) {
  const int       million = 1000000;
  time_t          sec,
                  usec;
  /* FIXME could be refactored to use timevalPlusMax */
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
  time_t          sec,
                  usec;
  /* FIXME could be refactored to use timevalMinusMax */
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
  result += 1000 * ru->ru_utime.tv_sec;
  result += 1000 * ru->ru_stime.tv_sec;
  result += ru->ru_utime.tv_usec / 1000;
  result += ru->ru_stime.tv_usec / 1000;
  return result;
}

/* Return time as number of milliseconds. */
uintmax_t getCurrentTime (void) {
  struct timeval tv;

  gettimeofday (&tv, (struct timezone *) NULL);
  return timevalTime (&tv);
}

void startTiming (struct rusage *ru_start) {
  getrusage (RUSAGE_SELF, ru_start);
}

uintmax_t stopTiming (struct rusage *ru_start, struct rusage *ru_acc) {
  struct rusage ru_finish, ru_total;

  getrusage (RUSAGE_SELF, &ru_finish);
  rusageMinusMax (&ru_finish, ru_start, &ru_total);
  rusagePlusMax (ru_acc, &ru_total, ru_acc);
  return rusageTime (&ru_total);
}


void timevalZero (struct timeval *tv) {
  memset (tv, 0, sizeof (*tv));
}

void timevalPlusMax (struct timeval *tv1,
                     struct timeval *tv2,
                     struct timeval *tv) {
  const int       million = 1000000;
  time_t          sec,
                  usec;

  sec = tv1->tv_sec + tv2->tv_sec;
  usec = tv1->tv_usec + tv2->tv_usec;
  sec += (usec / million);
  usec %= million;
  tv->tv_sec = sec;
  tv->tv_usec = usec;
}

void timevalMinusMax (struct timeval *tv1,
                      struct timeval *tv2,
                      struct timeval *tv) {
  const int       million = 1000000;
  time_t          sec,
                  usec;

  sec = (tv1->tv_sec - tv2->tv_sec) - 1;
  usec = tv1->tv_usec + million - tv2->tv_usec;
  sec += (usec / million);
  usec %= million;
  tv->tv_sec = sec;
  tv->tv_usec = usec;
}

uintmax_t timevalTime (struct timeval *tv) {
  uintmax_t result;

  result = 0;
  result += 1000 * tv->tv_sec;
  result += tv->tv_usec / 1000;
  return result;
}

void startWallTiming (struct timeval *tv_start) {
  gettimeofday (tv_start, (struct timezone *) NULL);
}

uintmax_t stopWallTiming (struct timeval *tv_start, struct timeval *tv_acc) {
  struct timeval tv_finish, tv_total;

  gettimeofday (&tv_finish, (struct timezone *) NULL);
  timevalMinusMax (&tv_finish, tv_start, &tv_total);
  timevalPlusMax (tv_acc, &tv_total, tv_acc);
  return timevalTime (&tv_total);
}
