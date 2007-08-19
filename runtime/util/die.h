/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* issue error message and exit */
extern void die (const char *fmt, ...)
                        __attribute__ ((format(printf, 1, 2)))
                        __attribute__ ((noreturn));
/* issue error message and exit.  Also print strerror(errno). */
extern void diee (const char *fmt, ...)
                        __attribute__ ((format(printf, 1, 2)))
                        __attribute__ ((noreturn));
