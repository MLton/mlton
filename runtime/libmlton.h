/* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 */
#ifndef _LIBMLTON_H
#define _LIBMLTON_H

#undef i386
#undef i486

#include "basis-constants.h"
#include "gc.h"
#include "IntInf.h"
#include "mlton-basis.h"
#include "mlton-posix.h"
#include "my-lib.h"
#include "posix-constants.h"

extern struct GC_state gcState;

/* initialize the machine */
void MLton_init(int argc, 
		char **argv,
		/* Read the globals from the world file. */
		void (*loadGlobals)(FILE *file));

/* Print a string, escaping every character with decimal escapes. */
void MLton_printStringEscaped(FILE *f, unsigned char *s);

#endif /* #ifndef _LIBMLTON_H */

