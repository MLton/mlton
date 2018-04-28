/* Copyright (C) 2016-2017 Matthew Fluet.
 * Copyright (C) 2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#include "platform.h"
struct GC_state gcState;

int main (__attribute__ ((unused)) int argc,
          __attribute__ ((unused)) char* argv[]) {
  FILE *sizesFd;

  sizesFd = fopen_safe ("sizes", "w");

  fprintf (sizesFd, "arrayMetaData = %"PRIuMAX"\n", (uintmax_t)GC_ARRAY_METADATA_SIZE);
  fprintf (sizesFd, "cint = %"PRIuMAX"\n",     (uintmax_t)sizeof(C_Int_t));
  fprintf (sizesFd, "cpointer = %"PRIuMAX"\n", (uintmax_t)sizeof(C_Pointer_t));
  fprintf (sizesFd, "cptrdiff = %"PRIuMAX"\n", (uintmax_t)sizeof(C_Ptrdiff_t));
  fprintf (sizesFd, "csize = %"PRIuMAX"\n",    (uintmax_t)sizeof(C_Size_t));
  fprintf (sizesFd, "header = %"PRIuMAX"\n",   (uintmax_t)sizeof(GC_header));
  fprintf (sizesFd, "mplimb = %"PRIuMAX"\n",   (uintmax_t)sizeof(C_MPLimb_t));
  fprintf (sizesFd, "normalMetaData = %"PRIuMAX"\n", (uintmax_t)GC_NORMAL_METADATA_SIZE);
  fprintf (sizesFd, "objptr = %"PRIuMAX"\n",   (uintmax_t)sizeof(objptr));
  fprintf (sizesFd, "seqIndex = %"PRIuMAX"\n", (uintmax_t)sizeof(GC_arrayLength));

  fclose_safe(sizesFd);

  return 0;
}
