/* Copyright (C) 2004-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */

#ifndef ASSERT
#define ASSERT 0
#endif

/* Assertion failure routine */
extern void asfail (char *file, int line, char *prop);

/* Assertion verifier */
#if ASSERT
#define	assert(p) ((p) ? (void)0 : asfail(__FILE__, __LINE__, #p))
#else
#define	assert(p) ((void)0)
#endif
