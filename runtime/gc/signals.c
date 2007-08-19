/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if not HAS_SIGALTSTACK

void initSignalStack (__attribute__ ((unused)) GC_state s) {
}

#else

void initSignalStack (GC_state s) {
  static stack_t altstack;
  size_t ss_size = align (SIGSTKSZ, s->sysvals.pageSize);
  size_t psize = s->sysvals.pageSize;
  void *ss_sp = GC_mmapAnon_safe_protect (NULL, 2 * ss_size, psize, psize);
  altstack.ss_sp = (void*)((pointer)ss_sp + ss_size);
  altstack.ss_size = ss_size;
  altstack.ss_flags = 0;
  sigaltstack (&altstack, NULL);
}

#endif
