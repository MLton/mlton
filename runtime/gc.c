/* Copyright (C) 2012,2014 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_FUNCS
#define MLTON_GC_INTERNAL_BASIS
#include "platform.h"

#if ASSERT
#define ARG_USED_FOR_ASSERT
#define LOCAL_USED_FOR_ASSERT
#else
#define ARG_USED_FOR_ASSERT  __attribute__ ((unused))
#define LOCAL_USED_FOR_ASSERT  __attribute__ ((unused))
#endif

#include "gc/virtual-memory.c"
#include "gc/align.c"
#include "gc/read_write.c"

#include "gc/array-allocate.c"
#include "gc/array.c"
#include "gc/atomic.c"
#include "gc/call-stack.c"
#include "gc/cheney-copy.c"
#include "gc/controls.c"
#include "gc/copy-thread.c"
#include "gc/current.c"
#include "gc/dfs-mark.c"
#include "gc/done.c"
#include "gc/enter_leave.c"
#include "gc/foreach.c"
#include "gc/forward.c"
#include "gc/frame.c"
#include "gc/garbage-collection.c"
#include "gc/gc_state.c"
#include "gc/generational.c"
#include "gc/handler.c"
#include "gc/hash-cons.c"
#include "gc/heap.c"
#include "gc/heap_predicates.c"
#include "gc/init-world.c"
#include "gc/init.c"
#include "gc/int-inf.c"
#include "gc/invariant.c"
#include "gc/mark-compact.c"
#include "gc/model.c"
#include "gc/new-object.c"
#include "gc/object-size.c"
#include "gc/object.c"
#include "gc/objptr.c"
#include "gc/pack.c"
#include "gc/pointer.c"
#include "gc/profiling.c"
#include "gc/rusage.c"
#include "gc/share.c"
#include "gc/signals.c"
#include "gc/size.c"
#include "gc/sources.c"
#include "gc/stack.c"
#include "gc/switch-thread.c"
#include "gc/thread.c"
#include "gc/translate.c"
#include "gc/weak.c"
#include "gc/world.c"
