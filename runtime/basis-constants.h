/* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
#ifndef _BASIS_CONSTANTS_H_
#define _BASIS_CONSTANTS_H_

#include <syslog.h>

#include <sys/time.h>
#include <sys/poll.h>
#if (defined (__linux__))
#include <sys/ptrace.h>
#endif
#include <sys/socket.h>

#include "gc.h"

/* ------------------------------------------------- */
/*                       Array                       */
/* ------------------------------------------------- */

#define Array_maxLen GC_MAX_ARRAY_LENGTH

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

#define Itimer_prof ITIMER_PROF
#define Itimer_real ITIMER_REAL
#define Itimer_virtual ITIMER_VIRTUAL

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

#if (defined (__sparc__))
#define MLton_Platform_Arch_host 0
#elif (defined (__i386__))
#define MLton_Platform_Arch_host 1
#else
#error MLton_Platform_Arch_host not defined
#endif

#if (defined (__CYGWIN__))
#define MLton_Platform_OS_host 0
#elif (defined (__FreeBSD__))
#define MLton_Platform_OS_host 1
#elif (defined (__linux__))
#define MLton_Platform_OS_host 2
#elif (defined (__NetBSD__))
#define MLton_Platform_OS_host 3
#elif (defined (__OpenBSD__))
#define MLton_Platform_OS_host 4
#elif (defined (__sun__))
#define MLton_Platform_OS_host 5
#else
#error MLton_Platform_OS_host not defined
#endif

#if (defined (__sun__))
#define LOG_AUTHPRIV LOG_AUTH
#define LOG_PERROR 0
#endif /* __sun__ */

/* ------------------------------------------------- */
/*                      OS                           */
/* ------------------------------------------------- */

#define OS_IO_POLLIN POLLIN
#define OS_IO_POLLPRI POLLPRI
#define OS_IO_POLLOUT POLLOUT

#endif /* #ifndef _BASIS_CONSTANTS_H_ */
