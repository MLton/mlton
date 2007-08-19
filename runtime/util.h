/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_UTIL_H_
#define _MLTON_UTIL_H_

#include "cenv.h"
#include "util/pointer.h"

#ifndef TRUE
#define TRUE    (0 == 0)
#endif
#ifndef FALSE
#define FALSE   (not TRUE)
#endif
#define unless(p)       if (not (p))
#define until(p)        while (not (p))
#define cardof(a)       (sizeof(a) / sizeof(*(a)))
#define endof(a)        ((a) + cardof(a))

#define TWOPOWER(n) (1 << (n))

#ifndef max
#define max(a, b) ((a)>(b)?(a):(b))
#endif

#ifndef min
#define min(a, b) ((a)<(b)?(a):(b))
#endif

#include "util/die.h"
#include "util/safe.h"
#include "util/read_write.h"
#include "util/to-string.h"
#include "util/align.h"
#include "util/endian.h"

#endif /* _MLTON_UTIL_H_ */
