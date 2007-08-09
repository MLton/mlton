#define MLTON_GC_INTERNAL_TYPES
#include "platform.h"
struct GC_state gcState;

int main (__attribute__ ((unused)) int argc, 
          __attribute__ ((unused)) char* argv[]) {
  FILE *sizesFd;

  sizesFd = fopen_safe ("sizes", "w");

  fprintf (sizesFd, "cint = %zu\n", sizeof(C_Int_t));
  fprintf (sizesFd, "cpointer = %zu\n", sizeof(C_Pointer_t));
  fprintf (sizesFd, "cptrdiff = %zu\n", sizeof(C_Ptrdiff_t));
  fprintf (sizesFd, "csize = %zu\n", sizeof(C_Size_t));
  fprintf (sizesFd, "header = %zu\n", sizeof(GC_header));
  fprintf (sizesFd, "mplimb = %zu\n", sizeof(C_MPLimb_t));
  fprintf (sizesFd, "objptr = %zu\n", sizeof(objptr));
  fprintf (sizesFd, "seqIndex = %zu\n", sizeof(GC_arrayLength));

  fclose_safe(sizesFd);

  return 0;
}
