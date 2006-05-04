/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef ASSERT
#define ASSERT 0
#endif

/* Assertion failure routine */
extern void asfail (const char *file, int line, const char *prop)
                        __attribute__ ((noreturn));

/* Assertion verifier */
#if ASSERT
#define assert(p) ((p) ? (void)0 : asfail(__FILE__, __LINE__, #p))
#else
#define assert(p) ((void)0)
#endif
