/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_EXPORT_H_
#define _MLTON_EXPORT_H_

/* ------------------------------------------------- */
/*                      Symbols                      */
/* ------------------------------------------------- */

/* An external symbol is something not defined by the module
 * (executable or library) being built. Rather, it is provided
 * from a library dependency (dll, dylib, or shared object).
 *
 * A public symbol is defined in this module as being available
 * to users outside of this module. If building a library, this 
 * means the symbol will be part of the public interface.
 * 
 * A private symbol is defined within this module, but will not
 * be made available outside of it. This is typically used for
 * internal implementation details that should not be accessible.
 */

#if defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__)
#define EXTERNAL __declspec(dllimport)
#define PUBLIC   __declspec(dllexport)
#define PRIVATE
#else
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#define EXTERNAL __attribute__((visibility("default")))
#define PUBLIC   __attribute__((visibility("default")))
#define PRIVATE  __attribute__((visibility("hidden")))
#else
#define EXTERNAL
#define PUBLIC
#define PRIVATE
#endif
#endif

#endif /* _MLTON_EXPORT_H_ */
