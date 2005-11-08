/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "platform.h"

Bool MLton_Platform_CygwinUseMmap = FALSE;

void GC_setCygwinUseMmap (bool b) {
  MLton_Platform_CygwinUseMmap = b;
}

extern char **environ; /* for Posix_ProcEnv_environ */

void MLton_init (int argc, char **argv, GC_state s) {
  int start;

  Posix_ProcEnv_environ = (CstringArray)environ;
  start = GC_init (s, argc, argv);
  /* Setup argv and argc that SML sees. */
  /* start is now the index of the first real arg. */
  CommandLine_commandName = (uint)(argv[0]);
  CommandLine_argc = argc - start;
  CommandLine_argv = (uint)(argv + start);
}

