/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Sequence objects have the following layout:
 * 
 * counter ::
 * length ::
 * header ::
 * ( (non heap-pointers)* :: (heap pointers)* )*
 *
 * The counter word is used by mark compact GC.  The length word is
 * the number of elements in the sequence.  Sequence elements have the same
 * individual layout as normal objects, omitting the header word.
 */

#define GC_SEQLEN_TYPE__(z) uint ## z ## _t
#define GC_SEQLEN_TYPE_(z) GC_SEQLEN_TYPE__(z)
#define GC_SEQLEN_TYPE GC_SEQLEN_TYPE_(GC_MODEL_SEQLEN_SIZE)
typedef GC_SEQLEN_TYPE GC_sequenceLength;
#define GC_SEQUENCE_LENGTH_SIZE sizeof(GC_sequenceLength)
#define PRIxSEQLEN__(z) PRIx ## z
#define PRIxSEQLEN_(z) PRIxSEQLEN__(z)
#define PRIxSEQLEN PRIxSEQLEN_(GC_MODEL_SEQLEN_SIZE)
#define FMTSEQLEN "%"PRIxSEQLEN
typedef GC_sequenceLength GC_sequenceCounter;
#define GC_SEQUENCE_COUNTER_SIZE sizeof(GC_sequenceCounter)
#define PRIxSEQCTR PRIxSEQLEN
#define FMTSEQCTR "%"PRIxSEQCTR
#define GC_SEQUENCE_METADATA_SIZE (GC_SEQUENCE_COUNTER_SIZE + GC_SEQUENCE_LENGTH_SIZE + GC_HEADER_SIZE)

COMPILE_TIME_ASSERT(sizeof_header__le__sizeof_seqlen,
                    sizeof(GC_header) <= sizeof(GC_sequenceLength));
COMPILE_TIME_ASSERT(sizeof_seqlen__eq__sizeof_seqctr,
                    sizeof(GC_sequenceLength) == sizeof(GC_sequenceCounter));


#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_sequenceLength* getSequenceLengthp (pointer a);
static inline GC_sequenceLength getSequenceLength (pointer a);
static inline GC_sequenceCounter* getSequenceCounterp (pointer a);
static inline GC_sequenceCounter getSequenceCounter (pointer a);

#if ASSERT
static inline pointer indexSequenceAtObjptrIndex (GC_state s, pointer a,
                                               		GC_sequenceCounter sequenceIndex,
                                               		uint32_t objptrIndex);
#endif

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

PRIVATE uintmax_t GC_getSequenceLength (pointer a);
PRIVATE void GC_sequenceCopy (GC_state s, pointer ad, size_t ds, pointer as, size_t ss, size_t l);
