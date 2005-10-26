/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* showMem displays the virtual memory mapping to stdout.  
 * It is used to diagnose memory problems. 
 */
void showMem (void);

void *GC_mmapAnon (void *start, size_t length);
void *GC_mmapAnon_safe (void *start, size_t length);
void *GC_mmap (void *start, size_t length);
void *GC_mmap_safe (void *start, size_t length);
void *GC_mmap_safe_protect (void *start, size_t length, 
                            size_t dead_low, size_t dead_high);
void GC_munmap (void *start, size_t length);
void *GC_mremap (void *start, size_t oldLength, size_t newLength);
void GC_release (void *base, size_t length);
void GC_decommit (void *base, size_t length);

void *getTextEnd (void);
void *getTextStart (void);
void setSigProfHandler (struct sigaction *sa);
