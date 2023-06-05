/* Copyright (C) 2019,2023 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * The "... reserved bytes ..." of a stack object constitute a linear
 * sequence of frames.  For the purposes of garbage collection and
 * profiling, we must be able to recover information (e.g., the size
 * and offsets of live heap-pointers) for each frame.  This data is
 * declared as follows:
 *
 *  GC_frameInfo *frameInfos;
 * 
 * The frameInfos pointer is initialized to point to a static array of
 * frame infos that is emitted for each compiled program.  The kind
 * field identifies whether or not the frame is for a C call.  (Note:
 * The ML stack is distinct from the system stack.  A C call executes
 * on the system stack.  The frame left on the ML stack is just a
 * marker.)  The offsets field points to an array (the zeroeth element
 * recording the size of the array) whose elements record byte offsets
 * from the bottom of the frame at which live heap pointers are
 * located.  The size field indicates the size of the frame, including
 * space for the return address.  The sourceSeqIndex field indicates
 * the sequence of source names corresponding to the frame as an index
 * into sourceSeqs; see sources.h.
 */
typedef const uint16_t *GC_frameOffsets;

typedef enum {
  CONT_FRAME,
  CRETURN_FRAME,
  FUNC_FRAME,
  HANDLER_FRAME
} GC_frameKind;

typedef const struct GC_frameInfo {
  const GC_frameKind kind;
  const GC_frameOffsets offsets;
  const uint16_t size;
  const GC_sourceSeqIndex sourceSeqIndex;
} *GC_frameInfo;
typedef uint32_t GC_frameIndex;
#define PRIFI PRIu32
#define FMTFI "%"PRIFI

typedef uintptr_t GC_returnAddress;
#define GC_RETURNADDRESS_SIZE sizeof(GC_returnAddress)
#define FMTRA "0x%016"PRIxPTR

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_frameIndex getFrameIndexFromReturnAddress (GC_state s, GC_returnAddress ra);
static inline GC_frameInfo getFrameInfoFromFrameIndex (GC_state s, GC_frameIndex findex);
static inline GC_frameInfo getFrameInfoFromReturnAddress (GC_state s, GC_returnAddress ra);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
