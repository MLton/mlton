/* Copyright (C) 2016-2017,2020 Matthew Fluet.
 * Copyright (C) 2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#include "platform.h"

#define MkSize(name, value)                                             \
  fprintf (stdout, "size::" #name " = %"PRIuMAX"\n", (uintmax_t)(value))

#define MkGCFieldOffset(field)                                          \
  fprintf (stdout, "offset::gcState." #field " = %"PRIuMAX"\n", (uintmax_t)(offsetof (struct GC_state, field)))

#define MkBoolConst(name)                                \
  fprintf (stdout, #name " = %s\n", name ? "true" : "false")

#define MkNumConst(name, ty)                                            \
  do {                                                                  \
    if ((double)((ty)(0.25)) > 0) {                                     \
      fprintf (stdout, #name " = %.20f\n", (double)name);     \
    } else if ((double)((ty)(-1)) > 0) {                                \
      fprintf (stdout, #name " = %"PRIuMAX"\n", (uintmax_t)name); \
    } else {                                                            \
      fprintf (stdout, #name " = %"PRIdMAX"\n", (intmax_t)name); \
    }                                                                   \
  } while (0)

#define MkStrConst(name)                                \
  fprintf (stdout, #name " = %s\n", name)

int main (__attribute__ ((unused)) int argc,
          __attribute__ ((unused)) char* argv[]) {

  MkSize (cint, sizeof(C_Int_t));
  MkSize (cpointer, sizeof(C_Pointer_t));
  MkSize (cptrdiff, sizeof(C_Ptrdiff_t));
  MkSize (csize, sizeof(C_Size_t));
  MkSize (header, sizeof(GC_header));
  MkSize (mplimb, sizeof(C_MPLimb_t));
  MkSize (normalMetaData, GC_NORMAL_METADATA_SIZE);
  MkSize (objptr, sizeof(objptr));
  MkSize (seqIndex, sizeof(GC_sequenceLength));
  MkSize (sequenceMetaData, GC_SEQUENCE_METADATA_SIZE);

  MkGCFieldOffset (atomicState);
  MkGCFieldOffset (exnStack);
  MkGCFieldOffset (frontier);
  MkGCFieldOffset (generationalMaps.cardMapAbsolute);
  MkGCFieldOffset (limit);
  MkGCFieldOffset (limitPlusSlop);
  MkGCFieldOffset (signalsInfo.signalIsPending);
  MkGCFieldOffset (sourceMaps.curSourceSeqIndex);
  MkGCFieldOffset (stackBottom);
  MkGCFieldOffset (stackLimit);
  MkGCFieldOffset (stackTop);

  MkStrConst (MLton_Platform_Arch_host);
  MkStrConst (MLton_Platform_OS_host);
  MkBoolConst (MLton_Platform_Arch_bigendian);
  #include "gen/gen-basis-ffi-consts.c"

  return 0;
}
