#include "platform.h"

#if (defined (RLIMIT_AS))
const C_Int_t MLton_Rlimit_AS = RLIMIT_AS;
#elif (defined (RLIMIT_DATA))
const C_Int_t MLton_Rlimit_AS = RLIMIT_DATA;
#else
#error MLton_Rlimit_AS not defined
#endif
const C_Int_t MLton_Rlimit_CORE = RLIMIT_CORE;
const C_Int_t MLton_Rlimit_CPU = RLIMIT_CPU;
const C_Int_t MLton_Rlimit_DATA = RLIMIT_DATA;
const C_Int_t MLton_Rlimit_FSIZE = RLIMIT_FSIZE;
const C_Int_t MLton_Rlimit_NOFILE = RLIMIT_NOFILE;
const C_Int_t MLton_Rlimit_STACK = RLIMIT_STACK;
const C_RLim_t MLton_Rlimit_INFINITY = RLIM_INFINITY;
