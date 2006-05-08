/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_CENV_H_
#define _MLTON_CENV_H_

/* GNU C Library Feature Macros */
#define _ISOC99_SOURCE
#define _BSD_SOURCE
// #define _XOPEN_SOURCE 600
/* Only enable _POSIX_C_SOURCE on platforms that don't have broken
 * system headers.
 */
#if (defined (__linux__))
#define _POSIX_C_SOURCE 200112L
#endif
#define _FILE_OFFSET_BITS 64

/* C99 headers */
// #include <assert.h>
// #include <complex.h>
#include <ctype.h>
#include <errno.h>
// #include <fenv.h>
#include <float.h>
#include <inttypes.h>
#include <iso646.h>
#include <limits.h>
// #include <locale.h>
#include <math.h>
// #include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
// stdint.h (or approximate equivalent) comes from the n-way OS switch below.
// #include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// #include <tgmath.h>
#include <time.h>
// #include <wchar.h>
// #include <wctype.h>

#define COMPILE_TIME_ASSERT(name, x) \
        typedef int _COMPILE_TIME_ASSERT___##name[(x) ? 1 : -1]
COMPILE_TIME_ASSERT(CHAR_BIT__is_eight, CHAR_BIT == 8);
COMPILE_TIME_ASSERT(sizeof_float__is_four, sizeof(float) == 4);
COMPILE_TIME_ASSERT(sizeof_double__is_eight, sizeof(double) == 8);
COMPILE_TIME_ASSERT(char__is_signed, (double)((char)(-1)) < 0);

#include <fcntl.h>
#include <unistd.h>

#include <dirent.h>
#include <poll.h>
#include <termios.h>
#include <utime.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>

#include "gmp.h"


#if (defined (__APPLE_CC__))
#define __Darwin__
#endif

#if (defined (__CYGWIN__))
#include "platform/cygwin.h"
#elif (defined (__Darwin__))
#include "platform/darwin.h"
#elif (defined (__FreeBSD__))
#include "platform/freebsd.h"
#elif (defined (__hpux__))
#include "platform/hpux.h"
#elif (defined (__linux__))
#include "platform/linux.h"
#elif (defined (__MINGW32__))
#include "platform/mingw.h"
#elif (defined (__NetBSD__))
#include "platform/netbsd.h"
#elif (defined (__OpenBSD__))
#include "platform/openbsd.h"
#elif (defined (__sun__))
#include "platform/solaris.h"
#else
#error unknown platform
#endif

#endif /* _MLTON_CENV_H_ */
