/* Copyright (C) 2010,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if not HAS_SIGALTSTACK

void initSignalStack (void) {
}

#else

void initSignalStack (void) {
  static stack_t altstack = { .ss_sp = NULL, .ss_size = 0, .ss_flags = 0 };

  if (! altstack.ss_sp) {
    size_t psize = GC_pageSize ();
    size_t ss_size = align (SIGSTKSZ, psize);
    int prot = PROT_READ | PROT_WRITE;
#if NEEDS_SIGALTSTACK_EXEC
    prot = prot | PROT_EXEC;
#endif
    void *ss_sp = GC_mmapAnonStack (NULL, 2 * ss_size, prot, psize, psize);
    altstack.ss_sp = (void*)((pointer)ss_sp + ss_size);
    altstack.ss_size = ss_size;
  }
  sigaltstack (&altstack, NULL);
}

#endif

void GC_initSignalStack (void) {
  initSignalStack ();
}
