/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_CENV_H_
#define _MLTON_CENV_H_

#define _ISOC99_SOURCE
#define _BSD_SOURCE

/* Only enable _POSIX_C_SOURCE on platforms that don't have broken system
 * headers.
 */
#if (defined (__linux__))
#define _POSIX_C_SOURCE 200112L
#endif

/* C99-specific headers */
#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdbool.h>
#include <iso646.h>
#include <stdint.h>
#include <inttypes.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <dirent.h>
#include <signal.h>
#include <termios.h>
#include <time.h>
#include <utime.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>


#include "gmp.h"

#endif /* _MLTON_CENV_H_ */
