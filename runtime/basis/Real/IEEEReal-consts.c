#include "platform.h"

#if not HAS_FPCLASSIFY
#ifndef FP_INFINITE
#define FP_INFINITE 1
#endif
#ifndef FP_NAN
#define FP_NAN 0
#endif
#ifndef FP_NORMAL
#define FP_NORMAL 4
#endif
#ifndef FP_SUBNORMAL
#define FP_SUBNORMAL 3
#endif
#ifndef FP_ZERO
#define FP_ZERO 2
#endif
#endif

const C_Int_t IEEEReal_FloatClass_FP_INFINITE = FP_INFINITE;
const C_Int_t IEEEReal_FloatClass_FP_NAN = FP_NAN;
const C_Int_t IEEEReal_FloatClass_FP_NORMAL = FP_NORMAL;
const C_Int_t IEEEReal_FloatClass_FP_SUBNORMAL = FP_SUBNORMAL;
const C_Int_t IEEEReal_FloatClass_FP_ZERO = FP_ZERO;


#define FE_NOSUPPORT -1

/* Can't handle undefined rounding modes with code like the following.
 *  #ifndef FE_TONEAREST
 *  #define FE_TONEAREST FE_NOSUPPORT
 *  #endif
 * On some platforms, FE_* are defined via an enum, not the
 * preprocessor, and hence don't show up as #defined.  In that case,
 * the below code overwrites them.
 */

#if not HAS_FEROUND
#ifndef FE_TONEAREST
#define FE_TONEAREST 0
#endif
#ifndef FE_DOWNWARD
#define FE_DOWNWARD 1
#endif
#ifndef FE_UPWARD
#define FE_UPWARD 2
#endif
#ifndef FE_TOWARDZERO
#define FE_TOWARDZERO 3
#endif
#endif

const C_Int_t IEEEReal_RoundingMode_FE_TONEAREST = FE_TONEAREST;
const C_Int_t IEEEReal_RoundingMode_FE_DOWNWARD = FE_DOWNWARD;
const C_Int_t IEEEReal_RoundingMode_FE_NOSUPPORT = FE_NOSUPPORT;
const C_Int_t IEEEReal_RoundingMode_FE_UPWARD = FE_UPWARD;
const C_Int_t IEEEReal_RoundingMode_FE_TOWARDZERO = FE_TOWARDZERO;
