/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_GC_H_
#define _MLTON_GC_H_

#include "cenv.h"
#include "util.h"

struct GC_state;
typedef struct GC_state *GC_state;

#if (defined (MLTON_GC_INTERNAL))

#define GC_MODEL_NATIVE32

#include "gc/align.h"
#include "gc/model.h"
#include "gc/pointer.h"
#include "gc/objptr.h"
#include "gc/object.h"
#include "gc/array.h"
#include "gc/frame.h"
#include "gc/stack.h"
#include "gc/thread.h"
#include "gc/weak.h"
#include "gc/int-inf.h"
#include "gc/string.h"
#include "gc/object-size.h"
#include "gc/generational.h"
#include "gc/heap.h"
#include "gc/current.h"
#include "gc/foreach.h"
#include "gc/translate.h"
#include "gc/sysvals.h"
#include "gc/controls.h"
#include "gc/major.h"
#include "gc/statistics.h"
#include "gc/forward.h"
#include "gc/cheney-copy.h"
#include "gc/hash-cons.h"
#include "gc/dfs-mark.h"
#include "gc/mark-compact.h"
#include "gc/invariant.h"
#include "gc/atomic.h"
#include "gc/enter_leave.h"
#include "gc/signals.h"
#include "gc/handler.h"
#include "gc/switch-thread.h"
#include "gc/garbage-collection.h"
#include "gc/new-object.h"
#include "gc/array-allocate.h"
#include "gc/sources.h"
#include "gc/call-stack.h"
#include "gc/profiling.h"
#include "gc/init-world.h"
#include "gc/world.h"
#include "gc/init.h"
#include "gc/copy-thread.h"
#include "gc/gc_state.h"

#else /* not (defined (MLTON_GC_INTERNAL)) */

struct GC_thread;
typedef struct GC_thread *GC_thread;
struct GC_profileData;
typedef struct GC_profileData *GC_profileData;

#endif /* (defined (MLTON_GC_INTERNAL)) */

#include "gc/debug.h"
#include "gc/platform.h"
#include "gc/gc_state_exports.h"
#include "gc/exports.h"

#endif /* _MLTON_GC_H_ */
