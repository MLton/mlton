/* Copyright (C) 2016-2017,2020 Matthew Fluet.
 * Copyright (C) 2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#include "platform.h"

int main (__attribute__ ((unused)) int argc,
          __attribute__ ((unused)) char* argv[]) {

  fprintf (stdout, "cint = %"PRIuMAX"\n",     (uintmax_t)sizeof(C_Int_t));
  fprintf (stdout, "cpointer = %"PRIuMAX"\n", (uintmax_t)sizeof(C_Pointer_t));
  fprintf (stdout, "cptrdiff = %"PRIuMAX"\n", (uintmax_t)sizeof(C_Ptrdiff_t));
  fprintf (stdout, "csize = %"PRIuMAX"\n",    (uintmax_t)sizeof(C_Size_t));
  fprintf (stdout, "header = %"PRIuMAX"\n",   (uintmax_t)sizeof(GC_header));
  fprintf (stdout, "mplimb = %"PRIuMAX"\n",   (uintmax_t)sizeof(C_MPLimb_t));
  fprintf (stdout, "normalMetaData = %"PRIuMAX"\n", (uintmax_t)GC_NORMAL_METADATA_SIZE);
  fprintf (stdout, "objptr = %"PRIuMAX"\n",   (uintmax_t)sizeof(objptr));
  fprintf (stdout, "seqIndex = %"PRIuMAX"\n", (uintmax_t)sizeof(GC_sequenceLength));
  fprintf (stdout, "sequenceMetaData = %"PRIuMAX"\n", (uintmax_t)GC_SEQUENCE_METADATA_SIZE);

  return 0;
}
