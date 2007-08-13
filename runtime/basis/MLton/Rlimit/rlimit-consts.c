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
/* NOT STANDARD */
#if (defined (RLIMIT_MEMLOCK))
const C_Int_t MLton_Rlimit_MEMLOCK = RLIMIT_MEMLOCK;
#else
const C_Int_t MLton_Rlimit_MEMLOCK = -1;
#endif
/* */
const C_Int_t MLton_Rlimit_NOFILE = RLIMIT_NOFILE;
/* NOT STANDARD */
#if (defined (RLIMIT_NPROC))
const C_Int_t MLton_Rlimit_NPROC = RLIMIT_NPROC;
#else
const C_Int_t MLton_Rlimit_NPROC = -1;
#endif
#if (defined (RLIMIT_RSS))
const C_Int_t MLton_Rlimit_RSS = RLIMIT_RSS;
#else
const C_Int_t MLton_Rlimit_RSS = -1;
#endif
/* */
const C_Int_t MLton_Rlimit_STACK = RLIMIT_STACK;
const C_RLim_t MLton_Rlimit_INFINITY = RLIM_INFINITY;
